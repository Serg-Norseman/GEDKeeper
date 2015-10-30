using System;
using System.Windows.Forms;

using GKCommon.GEDCOM.Enums;
using GKCore;

namespace GKUI.Dialogs
{
	/// <summary>
	/// 
	/// </summary>
	public partial class TfmSexCheck : Form
	{
		public TfmSexCheck()
		{
			this.InitializeComponent();

			// SetLang()
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
		
		public GEDCOMSex Sex
		{
			get
			{
			    if (this.sbMale.Checked) {
					return GEDCOMSex.svMale;
				}
			    if (this.sbFemale.Checked) {
			        return GEDCOMSex.svFemale;
			    }
			    return GEDCOMSex.svNone;
			}
		    set
			{
				switch (value) 
				{
					case GEDCOMSex.svNone:
					case GEDCOMSex.svUndetermined:
						this.sbNone.Checked = true;
						break;

					case GEDCOMSex.svMale:
						this.sbMale.Checked = true;
						break;

					case GEDCOMSex.svFemale:
						this.sbFemale.Checked = true;
						break;
				}
			}
		}
	}
}
