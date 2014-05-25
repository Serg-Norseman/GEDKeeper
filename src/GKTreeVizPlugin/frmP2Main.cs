using System;
using System.Windows.Forms;

using GKCore.Interfaces;

namespace GKTreeVizPlugin
{
    public partial class frmP2Main : Form
    {
        private readonly IPlugin plugin;

        public frmP2Main(IPlugin plugin)
        {
            InitializeComponent();
            
            this.plugin = plugin;
            this.Text = plugin.DisplayName;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            //MessageBox.Show(plugin.Description, "Информация", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }
    }
}
