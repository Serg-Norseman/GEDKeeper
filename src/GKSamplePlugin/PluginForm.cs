using System;
using System.Windows.Forms;

using GKCore.Interfaces;

namespace GKSamplePlugin
{
    public partial class PluginForm : Form
    {
        private readonly IPlugin plugin;

        public PluginForm(IPlugin plugin)
        {
            InitializeComponent();
            this.plugin = plugin;
        }

        private void frmP1Main_Load(object sender, EventArgs e)
        {
            tbInfo.AppendText(plugin.DisplayName + "\r\n");
        }
    }
}
