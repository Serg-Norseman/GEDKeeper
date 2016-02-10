using System;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Dialogs
{
	/// <summary>
	/// 
	/// </summary>
	public partial class TfmAssociationEdit : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;
		private GEDCOMAssociation fAssociation;
		private GEDCOMIndividualRecord fTempInd;

		public GEDCOMAssociation Association
		{
			get { return this.fAssociation; }
			set { this.SetAssociation(value); }
		}

		public IBaseWindow Base
		{
			get { return this.fBase; }
		}

		private void SetAssociation(GEDCOMAssociation value)
		{
			this.fAssociation = value;
			this.EditRelation.Text = this.fAssociation.Relation;
			string st = ((this.fAssociation.Individual == null) ? "" : this.fAssociation.Individual.GetNameString(true, false));
			this.EditPerson.Text = st;
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				string rel = this.EditRelation.Text.Trim();
				if (rel != "" && TfmGEDKeeper.Instance.Options.Relations.IndexOf(rel) < 0)
				{
					TfmGEDKeeper.Instance.Options.Relations.Add(rel);
				}

				this.fAssociation.Relation = this.EditRelation.Text;
				this.fAssociation.Individual = this.fTempInd;
				this.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmAssociationEdit.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnPersonAdd_Click(object sender, EventArgs e)
		{
			this.fTempInd = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
			this.EditPerson.Text = ((this.fTempInd == null) ? "" : this.fTempInd.GetNameString(true, false));
		}

		public TfmAssociationEdit(IBaseWindow aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			int num = TfmGEDKeeper.Instance.Options.Relations.Count;
			for (int i = 0; i < num; i++)
			{
				this.EditRelation.Items.Add(TfmGEDKeeper.Instance.Options.Relations[i]);
			}

			// SetLang()
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_Association);
			this.Label1.Text = LangMan.LS(LSID.LSID_Relation);
			this.Label2.Text = LangMan.LS(LSID.LSID_Person);
		}
	}
}
