using Eto.Drawing;
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
            tbImageSave.Image = Bitmap.FromResource("Resources.btn_save_image.gif");

            tbPrev = new ButtonToolItem();
            tbPrev.Enabled = false;
            tbPrev.Click += ToolBar1_ButtonClick;
            tbPrev.Image = Bitmap.FromResource("Resources.btn_left.gif");

            tbNext = new ButtonToolItem();
            tbNext.Enabled = false;
            tbNext.Click += ToolBar1_ButtonClick;
            tbNext.Image = Bitmap.FromResource("Resources.btn_right.gif");

            tbDocPreview = new ButtonToolItem();
            tbDocPreview.Click += tbDocPreview_Click;
            tbDocPreview.Image = Bitmap.FromResource("Resources.btn_preview.gif");

            tbDocPrint = new ButtonToolItem();
            tbDocPrint.Click += tbDocPrint_Click;
            tbDocPrint.Image = Bitmap.FromResource("Resources.btn_print.gif");

            tbOptions = new ButtonToolItem();
            tbOptions.Click += tbOptions_Click;
            tbOptions.Image = Bitmap.FromResource("Resources.btn_tools.gif");

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
