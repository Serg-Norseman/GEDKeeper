using System;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

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
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.SheetAuthor.Text = LangMan.LSList[142];
			this.Label1.Text = LangMan.LSList[85];
			this.Label2.Text = LangMan.LSList[82];
			this.Label3.Text = LangMan.LSList[131];
			this.SheetAdvanced.Text = LangMan.LS(LSID.LSID_Other);
		}
	}
}
