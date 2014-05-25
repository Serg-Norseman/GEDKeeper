using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmSexCheck : Form
	{
		public TfmSexCheck()
		{
			this.InitializeComponent();

			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_WinCheckSex);
			this.GroupBox1.Text = LangMan.LS(LSID.LSID_Sex);
			this.sbMale.Text = LangMan.LS(LSID.LSID_SexM);
			this.sbFemale.Text = LangMan.LS(LSID.LSID_SexF);
		}
		
		public string IndividualName
		{
			get { return this.edName.Text; }
			set { this.edName.Text = value; }
		}
		
		public TGEDCOMSex Sex
		{
			get
			{
				if (this.sbMale.Checked) {
					return TGEDCOMSex.svMale;
				} else if (this.sbFemale.Checked) {
					return TGEDCOMSex.svFemale;
				} else {
					return TGEDCOMSex.svNone;
				}
			}
			set
			{
				switch (value) 
				{
					case TGEDCOMSex.svNone:
					case TGEDCOMSex.svUndetermined:
						this.sbNone.Checked = true;
						break;

					case TGEDCOMSex.svMale:
						this.sbMale.Checked = true;
						break;

					case TGEDCOMSex.svFemale:
						this.sbFemale.Checked = true;
						break;
				}
			}
		}
	}
}
