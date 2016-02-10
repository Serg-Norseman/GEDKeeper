using System;
using System.Windows.Forms;

using GKCore.Interfaces;

namespace GKTreeVizPlugin
{
    public partial class frmP2Main : Form
    {
        private readonly IPlugin fPlugin;

        public frmP2Main(IPlugin fPlugin)
        {
            InitializeComponent();
            
            this.fPlugin = fPlugin;
            this.Text = fPlugin.DisplayName;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            //MessageBox.Show(plugin.Description, "Информация", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }
    }
}
