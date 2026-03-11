#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class MediaViewerWin
    {
        private void InitializeComponent()
        {
            Size = new Size(100, 60);
            Closing += MediaViewerWin_FormClosing;
            KeyDown += MediaViewerWin_KeyDown;
        }
    }
}
