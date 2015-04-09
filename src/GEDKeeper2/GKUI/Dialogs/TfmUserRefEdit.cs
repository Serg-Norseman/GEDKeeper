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
    public partial class TfmUserRefEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;

        private GEDCOMUserReference fUserRef;

		public GEDCOMUserReference UserRef
		{
			get { return this.fUserRef; }
			set { this.SetUserRef(value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
		}

		private void SetUserRef(GEDCOMUserReference value)
		{
			this.fUserRef = value;
			this.EditRef.Text = this.fUserRef.StringValue;
			this.EditType.Text = this.fUserRef.ReferenceType;
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.fUserRef.StringValue = this.EditRef.Text;
				this.fUserRef.ReferenceType = this.EditType.Text;
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmUserRefEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmUserRefEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			for (TUserRef ur = TUserRef.urCustom; ur <= TUserRef.urUSSR_RearVeteran; ur++)
			{
				this.EditRef.Items.Add(GKData.UserRefs[(int)ur]);
			}

			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_WinUserRefEdit);
			this.Label1.Text = LangMan.LS(LSID.LSID_Reference);
			this.Label2.Text = LangMan.LS(LSID.LSID_Type);
		}
	}
}
