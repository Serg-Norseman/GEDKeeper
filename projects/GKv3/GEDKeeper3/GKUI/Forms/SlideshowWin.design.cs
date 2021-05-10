using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class SlideshowWin
    {
        private ButtonToolItem tbNext;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbStart;
        private ToolBar toolStrip1;

        private void InitializeComponent()
        {
            SuspendLayout();

            tbStart = new ButtonToolItem();
            tbStart.Click += tsbStart_Click;

            tbPrev = new ButtonToolItem();
            tbPrev.Click += tsbPrev_Click;

            tbNext = new ButtonToolItem();
            tbNext.Click += tsbNext_Click;

            toolStrip1 = new ToolBar();
            toolStrip1.Items.AddRange(new ToolItem[] {
                                          tbStart,
                                          new SeparatorToolItem(),
                                          tbPrev,
                                          tbNext});
            ToolBar = toolStrip1;

            ShowInTaskbar = true;
            Title = "SlideshowWin";
            Load += SlideshowWin_Load;
            KeyDown += SlideshowWin_KeyDown;

            UIHelper.SetPredefProperties(this, 790, 570, true, true);
            ResumeLayout();
        }
    }
}
