using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKSys;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKUI
{
	public partial class TfmAssociationEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMAssociation FAssociation;
		private TGEDCOMIndividualRecord FTempInd;

		public TGEDCOMAssociation Association
		{
			get { return this.FAssociation; }
			set { this.SetAssociation(value); }
		}

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private void SetAssociation([In] TGEDCOMAssociation Value)
		{
			this.FAssociation = Value;
			this.EditRelation.Text = this.FAssociation.Relation;
			this.EditPerson.Text = TGenEngine.GetNameStr(this.FAssociation.Individual, true, false);
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				string rel = this.EditRelation.Text.Trim();
				if (rel != "" && GKUI.TfmGEDKeeper.Instance.Options.Relations.IndexOf(rel) < 0)
				{
					GKUI.TfmGEDKeeper.Instance.Options.Relations.Add(rel);
				}

				this.FAssociation.Relation = this.EditRelation.Text;
				this.FAssociation.Individual = this.FTempInd;
				this.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmAssociationEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnPersonAdd_Click(object sender, EventArgs e)
		{
			this.FTempInd = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svNone);
			this.EditPerson.Text = TGenEngine.GetNameStr(this.FTempInd, true, false);
		}

		public TfmAssociationEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			int num = GKUI.TfmGEDKeeper.Instance.Options.Relations.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				this.EditRelation.Items.Add(GKUI.TfmGEDKeeper.Instance.Options.Relations[i]);
			}

			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[94];
			this.Label1.Text = LangMan.LSList[95];
			this.Label2.Text = LangMan.LSList[96];
		}
	}
}
