using System;
using Eto.Drawing;
using GKUI.Components;

namespace GKUI
{
    partial class MediaViewerWin
    {
        private void InitializeComponent()
        {
            SuspendLayout();

            ClientSize = new Size(1027, 577);
            ShowInTaskbar = true;
            Title = "MediaViewerWin";
            Closing += MediaViewerWin_FormClosing;
            KeyDown += MediaViewerWin_KeyDown;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
