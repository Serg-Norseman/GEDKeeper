using System;
using System.Reflection;
using System.Windows.Forms;

using Ext.Utils;
using GKCore;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI
{
	public partial class TfmAbout : Form
	{
		public TfmAbout()
		{
			this.InitializeComponent();
			this.LabelCite.Text = "«История рода - это есть история Отечества»\r\n«Неуважение к предкам - есть первый признак дикости и безнравственности»\r\n(Александр Сергеевич Пушкин)";
			this.Text = LangMan.LSList[49];
			this.btnClose.Text = LangMan.LSList[99];

			//this.familyComboBox1.Doc = TfmGEDKeeper.Instance.GetCurrentFile().Tree;
		}

		private void LabelMail_Click(object sender, EventArgs e)
		{
			SysUtils.LoadExtFile(this.LabelMail.Text);
		}

		public static void ShowAbout(string AppName)
		{
			Assembly assembly = Assembly.GetExecutingAssembly();

			string copyright = "";
			object[] attributes = assembly.GetCustomAttributes(typeof(AssemblyCopyrightAttribute), false);
			if (attributes.Length != 0) copyright = ((AssemblyCopyrightAttribute)attributes[0]).Copyright;

			string version = assembly.GetName().Version.ToString();

			TfmAbout dlg = new TfmAbout();
			try
			{
				dlg.LabelProduct.Text = AppName;
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
