using System;
using System.Windows.Forms;
using GKCommon.Database;
using GKUI.Components;

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

            gkComboBox1.Items.Add(new GKComboItem("female", Resources.iSymFemale));
            gkComboBox1.Items.Add(new GKComboItem("male", Resources.iSymMale));
            gkComboBox1.Items.Add(new GKComboItem("undefined"));
        }

        private void MainFormFormClosed(object sender, FormClosedEventArgs e)
        {
            fDatabase.Disconnect();
        }

        private void Button1_Click(object sender, EventArgs e)
        {
            using (var frm = new MPlayerTest()) {
                frm.ShowDialog();
            }
        }
    }
}
