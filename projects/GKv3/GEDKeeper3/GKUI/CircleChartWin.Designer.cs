using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI
{
    partial class CircleChartWin
    {
        private ToolBar ToolBar1;
        private ButtonToolItem tbImageSave;
        private SeparatorToolItem tbs1;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private SeparatorToolItem tbs2;
        private ButtonToolItem tbDocPreview;
        private ButtonToolItem tbDocPrint;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fCircleChart.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            ToolBar1 = new ToolBar();
            tbImageSave = new ButtonToolItem();
            tbs1 = new SeparatorToolItem();
            tbPrev = new ButtonToolItem();
            tbNext = new ButtonToolItem();
            tbs2 = new SeparatorToolItem();
            tbDocPreview = new ButtonToolItem();
            tbDocPrint = new ButtonToolItem();
            SuspendLayout();

            ToolBar1.ImageScalingSize = new Size(20, 20);
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbImageSave,
                                        tbs1,
                                        tbPrev,
                                        tbNext,
                                        tbs2,
                                        tbDocPreview,
                                        tbDocPrint});
            ToolBar1.Location = new Point(0, 0);
            ToolBar1.Size = new Size(1093, 25);

            tbImageSave.Click += tbImageSave_Click;

            tbPrev.Enabled = false;
            tbPrev.Click += ToolBar1_ButtonClick;

            tbNext.Enabled = false;
            tbNext.Click += ToolBar1_ButtonClick;

            tbDocPreview.Text = "ButtonToolItem1";
            tbDocPreview.Click += tbDocPreview_Click;

            tbDocPrint.Text = "ButtonToolItem2";
            tbDocPrint.Click += tbDocPrint_Click;

            ClientSize = new Size(1093, 579);
            Controls.Add(ToolBar1);
            ShowInTaskbar = false;
            Title = "CircleChartWin";
            KeyDown += CircleChartWin_KeyDown;
            ResumeLayout();
        }
    }
}
