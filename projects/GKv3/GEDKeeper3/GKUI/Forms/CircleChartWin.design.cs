using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class CircleChartWin
    {
        private ToolBar ToolBar1;
        private ButtonToolItem tbImageSave;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private ButtonToolItem tbDocPreview;
        private ButtonToolItem tbDocPrint;
        private ButtonToolItem tbOptions;

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
            tbDocPreview.Click += tbDocPreview_Click;

            tbDocPrint = new ButtonToolItem();
            tbDocPrint.Click += tbDocPrint_Click;

            tbOptions = new ButtonToolItem();
            tbOptions.Click += tbOptions_Click;

            ToolBar1 = new ToolBar();
            ToolBar1.TextAlign = ToolBarTextAlign.Right;
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbImageSave,
                                        new SeparatorToolItem(),
                                        tbPrev,
                                        tbNext,
                                        new SeparatorToolItem(),
                                        tbDocPreview,
                                        tbDocPrint,
                                        new SeparatorToolItem(),
                                        tbOptions});

            ShowInTaskbar = true;
            Title = "CircleChartWin";
            ToolBar = ToolBar1;
            KeyDown += CircleChartWin_KeyDown;

            UIHelper.SetPredefProperties(this, 1100, 580, true, true);
            ResumeLayout();
        }
    }
}
