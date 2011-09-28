using System;
using System.Windows.Forms;

using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmAbout : Form
	{
		public TfmAbout()
		{
			this.InitializeComponent();
			this.LabelCite.Text = "«История рода - это есть история Отечества»\r\n«Неуважение к предкам - есть первый признак дикости и безнравственности»\r\n(Александр Сергеевич Пушкин)";
			this.Text = GKL.LSList[49];
			this.btnClose.Text = GKL.LSList[99];
		}

		private void LabelMail_Click(object sender, EventArgs e)
		{
			SysUtils.LoadExtFile(this.LabelMail.Text);
		}

		public static void ShowAbout(string AppName, string AppVersion)
		{
			TfmAbout dlg = new TfmAbout();
			try
			{
				dlg.LabelProduct.Text = AppName;
				dlg.LabelVersion.Text = "Version " + AppVersion;
				dlg.ShowDialog();
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
		}

	}
}
