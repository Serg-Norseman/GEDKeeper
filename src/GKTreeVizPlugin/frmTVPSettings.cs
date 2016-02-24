using System;
using System.Windows.Forms;

using GKCore.Interfaces;

namespace GKTreeVizPlugin
{
    public partial class frmTVPSettings : Form
    {
        private readonly IPlugin fPlugin;

        public int MinGens
        {
        	get { return decimal.ToInt32(this.edMinGens.Value); }
        }
        
        public frmTVPSettings(IPlugin fPlugin)
        {
            InitializeComponent();
            
            this.fPlugin = fPlugin;
            this.Text = fPlugin.DisplayName;
        }
        
        private void BtnAcceptClick(object sender, EventArgs e)
        {
        	
        }
    }
}
