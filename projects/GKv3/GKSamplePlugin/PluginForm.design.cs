using Eto.Drawing;
using Eto.Forms;

namespace GKSamplePlugin
{
    partial class PluginForm
    {
        private TextBox tbInfo;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            tbInfo = new TextBox();
            SuspendLayout();

            //tbInfo.Multiline = true;
            tbInfo.ReadOnly = true;

            ClientSize = new Size(160, 100);
            Content = (tbInfo);
            Title = "frmP1Main";
            Load += frmP1Main_Load;
            ResumeLayout();
        }
    }
}