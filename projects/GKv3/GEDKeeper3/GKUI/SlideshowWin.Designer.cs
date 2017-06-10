using System;
using System.Timers;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI
{
    partial class SlideshowWin
    {
        private Timer timer1;
        private ButtonToolItem tbNext;
        private ButtonToolItem tbPrev;
        private SeparatorToolItem SeparatorToolItem1;
        private ButtonToolItem tbStart;
        private ToolBar toolStrip1;

        private void InitializeComponent()
        {
            toolStrip1 = new ToolBar();
            tbStart = new ButtonToolItem();
            SeparatorToolItem1 = new SeparatorToolItem();
            tbPrev = new ButtonToolItem();
            tbNext = new ButtonToolItem();
            timer1 = new Timer();
            SuspendLayout();

            toolStrip1.ImageScalingSize = new Size(20, 20);
            toolStrip1.Items.AddRange(new ToolItem[] {
                                          tbStart,
                                          SeparatorToolItem1,
                                          tbPrev,
                                          tbNext});
            toolStrip1.Location = new Point(0, 0);
            toolStrip1.Text = "toolStrip1";

            tbStart.Text = "tbStart";
            tbStart.Click += tsbStart_Click;

            tbPrev.Text = "tbPrev";
            tbPrev.Click += tsbPrev_Click;

            tbNext.Text = "tbNext";
            tbNext.Click += tsbNext_Click;

            timer1.Interval = 1000;
            timer1.Tick += Timer1Tick;

            ClientSize = new Size(792, 573);
            Controls.Add(toolStrip1);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            ShowInTaskbar = false;
            Title = "SlideshowWin";
            Load += SlideshowWin_Load;
            KeyDown += SlideshowWin_KeyDown;
            ResumeLayout();
        }
    }
}
