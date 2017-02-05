using System;
using System.Windows.Forms;
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
        }

        private void MainFormFormClosed(object sender, FormClosedEventArgs e)
        {
            fDatabase.Disconnect();
        }
    }
}
