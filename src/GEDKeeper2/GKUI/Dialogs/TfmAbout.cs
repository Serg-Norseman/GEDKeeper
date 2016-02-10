using System;
using System.Windows.Forms;
using GKCore;

namespace GKUI.Dialogs
{
	/// <summary>
	/// 
	/// </summary>
	public partial class TfmAbout : Form
	{
		public TfmAbout()
		{
			this.InitializeComponent();
			this.LabelCite.Text = GKData.AppCites;
			this.Text = LangMan.LS(LSID.LSID_MIAbout);
			this.btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
		}

		private void LabelMail_Click(object sender, EventArgs e)
		{
			SysUtils.LoadExtFile(this.LabelMail.Text);
		}

		public static void ShowAbout()
		{
			string copyright, version;
			GKUtils.GetAssemblyVersion(out copyright, out version);

			TfmAbout dlg = new TfmAbout();
			try
			{
				dlg.LabelProduct.Text = GKData.AppTitle;
				dlg.LabelVersion.Text = "Version " + version;
				dlg.LabelCopyright.Text = copyright;
				dlg.ShowDialog();
			}
			finally
			{
				dlg.Dispose();
			}
		}
	}
}
