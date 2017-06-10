using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI
{
    partial class MediaViewerWin
    {
        private void InitializeComponent()
        {
            SuspendLayout();

            ClientSize = new Size(1027, 577);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            ShowInTaskbar = false;
            Title = "MediaViewerWin";
            FormClosing += MediaViewerWin_FormClosing;
            KeyDown += MediaViewerWin_KeyDown;
            ResumeLayout();
        }
    }
}
