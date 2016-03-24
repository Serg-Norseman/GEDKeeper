using System;
using System.Windows.Forms;

using GKCore.Interfaces;
using GKCore.Types;

namespace GKPedigreeImporterPlugin
{
    public partial class frmPedigreeImporter : Form
    {
        private readonly IPlugin fPlugin;
        private readonly ILangMan fLangMan;
        private readonly IBaseWindow fBase;
        private readonly Importer fImporter;
        private int fCurrentStage;
        private int fAvailableStage;

        public frmPedigreeImporter(IPlugin fPlugin)
        {
            InitializeComponent();

			this.fPlugin = fPlugin;
            this.fLangMan = fPlugin.LangMan;

			this.fBase = fPlugin.Host.GetCurrentFile();
			this.fImporter = new Importer(fBase, this.fLangMan, this.lbLog.Items);
            this.fCurrentStage = 0;
            this.fAvailableStage = 0;

            this.cbPersonSeparator.SelectedIndex = 0;
            this.cbNameFormat.SelectedIndex = 0;
            this.cbGenerationFormat.SelectedIndex = 0;
            this.cbDatesFormat.SelectedIndex = 0;
            this.cbDateSeparator.SelectedIndex = 0;

            // SetLang()
            this.Text = fLangMan.LS(ILS.LSID_PluginTitle);
            this.lblFile.Text = fLangMan.LS(ILS.LSID_File);
            this.btnImportFileChoose.Text = fLangMan.LS(ILS.LSID_DlgSelect) + @"...";
        }

		private void btnImportFileChoose_Click(object sender, EventArgs e)
		{
			if (this.OpenDialog2.ShowDialog() == DialogResult.OK)
			{
				this.edImportFile.Text = this.OpenDialog2.FileName;

				try
				{
					bool res = this.fImporter.LoadRawData(this.edImportFile.Text);
					if (res) {
						this.fAvailableStage++;

						switch (this.fImporter.CanNumbersType)
						{
							case PersonNumbersType.pnUndefined:
								rbNumsUnknown.Checked = true;
								break;
							case PersonNumbersType.pnKonovalov:
								rbNumsKonovalov.Checked = true;
								break;
							case PersonNumbersType.pnDAboville:
								rbNumsDAboville.Checked = true;
								break;
						}

					}

					this.UpdateNavigation();
				}
				catch (Exception ex)
				{
					MessageBox.Show(ex.Message, fLangMan.LS(ILS.LSID_PluginTitle), MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			}
		}

		private void ChangeStage()
		{
			switch (this.fCurrentStage)
			{
				case 1:
					{
						if (rbNumsUnknown.Checked) {
							this.fImporter.NumbersType = PersonNumbersType.pnUndefined;
						} else if (rbNumsKonovalov.Checked) {
							this.fImporter.NumbersType = PersonNumbersType.pnKonovalov;
						} else if (rbNumsDAboville.Checked) {
							this.fImporter.NumbersType = PersonNumbersType.pnDAboville;
						}

						if (cbPersonSeparator.SelectedIndex == 0) {
							this.fImporter.PersonLineSeparator = (char)0;
						} else {
							this.fImporter.PersonLineSeparator = cbPersonSeparator.Text[0];
						}

						this.fImporter.NameFormat = (NameFormat)cbNameFormat.SelectedIndex;
						this.fImporter.GenerationFormat = (GenerationFormat)cbGenerationFormat.SelectedIndex;
						this.fImporter.DateFormat = (DateFormat)cbDatesFormat.SelectedIndex;
						this.fImporter.PersonLineSeparator = cbDateSeparator.Text[0];

						this.fImporter.SurnamesNormalize = chkSurnamesNormalize.Checked;

						this.fImporter.SpecialFormat_1 = chkSpecial_1.Checked;

						this.fImporter.ImportContent();

						this.fBase.RefreshLists(false);
					}
					break;
			}
		}

		private void UpdateNavigation()
		{
			btnBack.Enabled = (this.fCurrentStage > 0);
			btnNext.Enabled = (this.fCurrentStage < this.fAvailableStage);
		}

        private void btnBack_Click(object sender, EventArgs e)
        {
        	this.fCurrentStage--;
        	this.tabControl1.SelectedTab = this.tabControl1.TabPages[this.fCurrentStage];
        	this.UpdateNavigation();
        }

        private void btnNext_Click(object sender, EventArgs e)
        {
        	this.fCurrentStage++;
        	this.tabControl1.SelectedTab = this.tabControl1.TabPages[this.fCurrentStage];
        	this.UpdateNavigation();
        	this.ChangeStage();
        }

        private void rbNums_CheckedChanged(object sender, EventArgs e)
        {
        	RadioButton rb = sender as RadioButton;
        	if (rb == this.rbNumsUnknown && rb.Checked) {
        		
        	}
        }

        private void btnClose_Click(object sender, EventArgs e)
        {
        	this.Close();
        }
    }
}
