using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class ProgressDlg
    {
        private Label lblTimePassed;
        private Label lblTimeRemain;
        private Label lblPassedVal;
        private Label lblRemainVal;
        private Label lblTimeTotal;
        private Label lblTotalVal;
        private ProgressBar ProgressBar1;
        private Label lblTitle;
        private Button btnCancel;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblTitle = new Label();
            lblTitle.Text = "lblTitle";

            ProgressBar1 = new ProgressBar();
            ProgressBar1.Size = new Size(380, 20);

            lblTimePassed = new Label();
            lblTimePassed.Text = "lblTimePassed";

            lblTimeRemain = new Label();
            lblTimeRemain.Text = "lblTimeRemain";

            lblTimeTotal = new Label();
            lblTimeTotal.Text = "lblTimeTotal";

            lblPassedVal = new Label();
            lblPassedVal.Text = "lblPassedVal";
            lblPassedVal.TextAlignment = TextAlignment.Right;

            lblRemainVal = new Label();
            lblRemainVal.Text = "lblRemainVal";
            lblRemainVal.TextAlignment = TextAlignment.Right;

            lblTotalVal = new Label();
            lblTotalVal.Text = "lblTotalVal";
            lblTotalVal.TextAlignment = TextAlignment.Right;

            var timesPanel = new TableLayout {
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblTimePassed, lblPassedVal }
                    },
                    new TableRow {
                        Cells = { lblTimeRemain, lblRemainVal }
                    },
                    new TableRow {
                        Cells = { lblTimeTotal, lblTotalVal }
                    }
                }
            };

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblTitle }
                    },
                    new TableRow {
                        Cells = { ProgressBar1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { timesPanel }
                    },
                    UIHelper.MakeDialogFooter(null, btnCancel)
                }
            };

            Maximizable = false;
            Minimizable = false;
            Resizable = false;
            ShowInTaskbar = false;
            Title = "ProgressDlg";
            Topmost = true;

            UIHelper.SetPredefProperties(this, 400, 150);
            ResumeLayout();
        }
    }
}
