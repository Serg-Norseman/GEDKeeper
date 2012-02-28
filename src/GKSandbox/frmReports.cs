using System;
using System.Diagnostics;
using System.Windows.Forms;

using GedCom551;

/// <summary>
/// Localization: unknown
/// CodeTransformation: need
/// </summary>

namespace GKSandbox
{
	public enum ReportType
	{
		rtTableAll,
		rtPagesAll,
		rtFamilyBook
	}

	public partial class frmReports : Form
	{
		private TGEDCOMTree tree;
		private ReportProperties repProperties;
		private ReportFactory reportFactory;
		private string lastReport;

		public frmReports(TGEDCOMTree tree)
		{
			this.InitializeComponent();
			this.tree = tree;
			this.repProperties = new ReportProperties();
			this.reportFactory = new ReportFactory(this.tree, this.repProperties);
		}

		void OnShown(object sender, EventArgs e)
		{
			this.btnShowReport.Enabled = false;
		}

		private void CreateReport(string filename, ReportType repType)
		{
			this.toolStripStatusLabel1.Text = "Генерация отчета... пожалуйста, подождите...";
			this.statusStrip1.Refresh();
			Cursor prev_cursor = this.Cursor;
			this.Cursor = Cursors.WaitCursor;

			this.timer1.Start();

			this.reportFactory.Caption = this.textBox3.Text;

			bool res = true;

			switch (repType) {
				case ReportType.rtTableAll:
					res	= reportFactory.CreateReport_PersonsTable(filename);
					break;
				case ReportType.rtPagesAll:
					res	= reportFactory.CreateReport_PersonsPages(filename, "");
					break;
				case ReportType.rtFamilyBook:
					res	= reportFactory.CreateReport_FamilyBook(filename);
					break;
			}

			if (!res) MessageBox.Show(reportFactory.Message);

			this.timer1.Stop();

			this.btnShowReport.Enabled = true;
			this.toolStripStatusLabel1.Text = "Отчет завершен.";
			this.Cursor = prev_cursor;
		}

		void btnCreateReport_Click(object sender, EventArgs e)
		{
			this.saveFileDialog1.Filter = "PDF files (*.pdf)|*.pdf|HTML files (*.htm)|*.htm|RTF files (*.rtf)|*.rtf";

			if (this.saveFileDialog1.ShowDialog() == DialogResult.OK)
			{
				string fileName = this.saveFileDialog1.FileName;
				ReportType repType = ReportType.rtTableAll;

				if (this.radioButton1.Checked) { repType = ReportType.rtTableAll; }
				else
				if (this.radioButton2.Checked) { repType = ReportType.rtPagesAll; }
				else
				if (this.radioButton3.Checked) { repType = ReportType.rtFamilyBook; }

				this.CreateReport(fileName, repType);

				this.lastReport = fileName;
			}
		}

		void btnShowReport_Click(object sender, EventArgs e)
		{
			if (System.IO.File.Exists(this.lastReport)) Process.Start(this.lastReport);
		}

		void OnTick(object sender, EventArgs e)
		{
			this.toolStripStatusLabel1.Text = this.reportFactory.PagesCreated.ToString() + " страниц";
			this.statusStrip1.Refresh();
		}

		void btnSettings_Click(object sender, EventArgs e)
		{
			frmReportsSetup frmReportsSetup = new frmReportsSetup(this.repProperties);
			frmReportsSetup.ShowDialog();
		}

		void btnTableSettings_Click(object sender, EventArgs e)
		{
			frmFieldsSetup frmFields = new frmFieldsSetup(this.repProperties.Table);
			frmFields.ShowDialog();
		}

	}
}
