using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmFileProperties : Form
	{
		private TfmBase FBase;

		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		private void UpdateControls()
		{
			TGEDCOMSubmitterRecord submitter = this.Base.Engine.GetSubmitter();
			this.EditName.Text = submitter.Name.FullName;
			this.MemoAddress.Text = submitter.Address.Address.Text;
			this.EditTel.Text = submitter.Address.GetPhoneNumber(0);
			this.CheckAdvanced.Checked = this.Base.Engine.IsAdvanced;
			this.edExtName.Text = this.Base.Engine.GetSpecExtName();
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				TGEDCOMSubmitterRecord submitter = this.Base.Engine.GetSubmitter();
				submitter.Name.StringValue = this.EditName.Text;
				submitter.Address.SetAddressArray(this.MemoAddress.Lines);
				submitter.Address.SetPhoneNumber(0, this.EditTel.Text);
				submitter.ChangeDate.ChangeDateTime = DateTime.Now;
				this.Base.Engine.IsAdvanced = this.CheckAdvanced.Checked;
				this.Base.Engine.ExtName = this.edExtName.Text;
				this.Base.Modified = true;
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmFileProperties.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmFileProperties(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.UpdateControls();
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.SheetAuthor.Text = GKL.LSList[142];
			this.Label1.Text = GKL.LSList[85];
			this.Label2.Text = GKL.LSList[82];
			this.Label3.Text = GKL.LSList[131];
			this.SheetAdvanced.Text = GKL.LSList[167];
			this.CheckAdvanced.Text = GKL.LSList[168];
			this.Label4.Text = GKL.LSList[169];
		}
	}
}
