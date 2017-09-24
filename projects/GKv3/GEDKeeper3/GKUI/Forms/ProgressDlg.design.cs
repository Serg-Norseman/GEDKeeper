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

        private void InitializeComponent()
        {
            SuspendLayout();

            lblTitle = new Label();
            lblTitle.Text = "lblTitle";

            ProgressBar1 = new ProgressBar();

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
                    }
                }
            };

            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "ProgressDlg";
            Topmost = true;

            UIHelper.SetPredefProperties(this, 400, 150);
            ResumeLayout();
        }
    }
}
