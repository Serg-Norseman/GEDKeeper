using System;
using System.Windows.Forms;
using GKCommon;
using GKCommon.Controls;
using GKCommon.Database;

namespace GKSandbox
{
    public partial class MainForm : Form
    {
        private GKDatabase fDatabase;

        public MainForm()
        {
            InitializeComponent();

            fDatabase = new GKDatabase();
            fDatabase.Connect();

            gkComboBox1.Items.Add(new DropDownItem("female", Resources.symbol_female));
            gkComboBox1.Items.Add(new DropDownItem("male", Resources.symbol_male));
            gkComboBox1.Items.Add(new DropDownItem("undefined"));
        }

        private void MainFormFormClosed(object sender, FormClosedEventArgs e)
        {
            fDatabase.Disconnect();
        }

        private void btnOpen_Click(object sender, EventArgs e)
        {
            using (var ofd = new OpenFileDialog())
            {
                if (ofd.ShowDialog() == DialogResult.OK)
                {
                    txtFileName.Text = ofd.FileName;
                    mediaPlayer1.MediaFile = ofd.FileName;
                }
            }
        }
    }
}
