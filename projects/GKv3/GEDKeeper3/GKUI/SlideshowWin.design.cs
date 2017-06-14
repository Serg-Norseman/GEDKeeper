using System;
using System.Timers;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI
{
    partial class SlideshowWin
    {
        private Timer timer1;
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

            timer1 = new Timer();
            timer1.Interval = 1000;
            timer1.Elapsed += Timer1Tick;

            toolStrip1 = new ToolBar();
            toolStrip1.Items.AddRange(new ToolItem[] {
                                          tbStart,
                                          new SeparatorToolItem(),
                                          tbPrev,
                                          tbNext});
            ToolBar = toolStrip1;

            ClientSize = new Size(792, 573);
            ShowInTaskbar = true;
            Title = "SlideshowWin";
            Load += SlideshowWin_Load;
            KeyDown += SlideshowWin_KeyDown;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
