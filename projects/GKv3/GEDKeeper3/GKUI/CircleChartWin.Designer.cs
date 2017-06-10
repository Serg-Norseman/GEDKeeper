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

        private void InitializeComponent()
        {
            SuspendLayout();

            tbImageSave = new ButtonToolItem();
            tbImageSave.Click += tbImageSave_Click;

            tbPrev = new ButtonToolItem();
            tbPrev.Enabled = false;
            tbPrev.Click += ToolBar1_ButtonClick;

            tbNext = new ButtonToolItem();
            tbNext.Enabled = false;
            tbNext.Click += ToolBar1_ButtonClick;

            tbDocPreview = new ButtonToolItem();
            tbDocPreview.Text = "ButtonToolItem1";
            tbDocPreview.Click += tbDocPreview_Click;

            tbDocPrint = new ButtonToolItem();
            tbDocPrint.Text = "ButtonToolItem2";
            tbDocPrint.Click += tbDocPrint_Click;

            ToolBar1 = new ToolBar();
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbImageSave,
                                        new SeparatorToolItem(),
                                        tbPrev,
                                        tbNext,
                                        new SeparatorToolItem(),
                                        tbDocPreview,
                                        tbDocPrint});

            ClientSize = new Size(1093, 579);
            ShowInTaskbar = false;
            Title = "CircleChartWin";
            KeyDown += CircleChartWin_KeyDown;

            ResumeLayout();
        }
    }
}
