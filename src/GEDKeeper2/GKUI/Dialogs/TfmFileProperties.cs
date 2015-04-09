using System;
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmFileProperties : Form, IBaseEditor
	{
		private readonly IBase fBase;

		public IBase Base
		{
			get { return this.fBase; }
		}

        public TfmFileProperties(IBase aBase)
        {
            this.InitializeComponent();

            this.fBase = aBase;

            this.UpdateControls();

            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.SheetAuthor.Text = LangMan.LS(LSID.LSID_Author);
            this.Label1.Text = LangMan.LS(LSID.LSID_Name);
            this.Label2.Text = LangMan.LS(LSID.LSID_Address);
            this.Label3.Text = LangMan.LS(LSID.LSID_Telephone);
            this.SheetAdvanced.Text = LangMan.LS(LSID.LSID_Other);
        }

		private void UpdateControls()
		{
			GEDCOMSubmitterRecord submitter = this.fBase.Tree.aux_GetSubmitter();
			this.EditName.Text = submitter.Name.FullName;
			this.MemoAddress.Text = submitter.Address.Address.Text;

			if (submitter.Address.PhoneNumbers.Count > 0) {
				this.EditTel.Text = submitter.Address.PhoneNumbers[0].StringValue;
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				GEDCOMSubmitterRecord submitter = this.fBase.Tree.aux_GetSubmitter();
				submitter.Name.StringValue = this.EditName.Text;
				submitter.Address.SetAddressArray(this.MemoAddress.Lines);

				if (submitter.Address.PhoneNumbers.Count > 0) {
					submitter.Address.PhoneNumbers[0].StringValue = this.EditTel.Text;
				} else {
					submitter.Address.AddPhoneNumber(this.EditTel.Text);
				}

				submitter.ChangeDate.ChangeDateTime = DateTime.Now;
				this.fBase.Modified = true;
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmFileProperties.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}
	}
}
