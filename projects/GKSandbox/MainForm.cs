using System;
using System.Windows.Forms;
using GKCommon;
using GKCommon.Controls;
using GKCommon.Database;

namespace GKSandbox
{
    public partial class MainForm : Form
    {
        GKDatabase fDatabase;

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
    }
}
