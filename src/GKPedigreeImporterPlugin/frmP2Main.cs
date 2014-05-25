using System;
using System.Windows.Forms;
using GKCore.Interfaces;

namespace GKPedigreeImporterPlugin
{
    public partial class frmP2Main : Form
    {
        private readonly IPlugin plugin;
        private readonly ILangMan fLangMan;

        public frmP2Main(IPlugin plugin)
        {
            InitializeComponent();

			this.plugin = plugin;
            this.fLangMan = plugin.LangMan;
            
            this.Text = fLangMan.LS(ILS.LSID_PluginTitle);
            this.Label3.Text = fLangMan.LS(ILS.LSID_File);
            this.btnImportFileChoose.Text = fLangMan.LS(ILS.LSID_DlgSelect) + "...";
        }

		void btnImportFileChoose_Click(object sender, EventArgs e)
		{
			IBase curBase = plugin.Host.GetCurrentFile(false);
			
			if (this.OpenDialog2.ShowDialog() == DialogResult.OK)
			{
				this.edImportFile.Text = this.OpenDialog2.FileName;
				Importer imp = new Importer(curBase, this.fLangMan, this.ListBox1.Items);
				try
				{
					imp.TreeImportEx(this.edImportFile.Text);
				}
				finally
				{
					imp.Dispose();
				}
				this.ListBox1.SelectedIndex = this.ListBox1.Items.Count - 1;

				curBase.RefreshLists(false);
			}
		}

    }
}
