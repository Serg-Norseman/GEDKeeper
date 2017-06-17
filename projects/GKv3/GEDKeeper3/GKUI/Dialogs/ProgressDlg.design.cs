using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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

            ProgressBar1 = new ProgressBar();
            //ProgressBar1.Step = 1;
            //ProgressBar1.Style = ProgressBarStyle.Continuous;

            lblTitle = new Label();
            //lblTitle.Size = new Size(47, 17);
            lblTitle.Text = "lblTitle";

            lblTimePassed = new Label();
            //lblTimePassed.Size = new Size(117, 17);
            lblTimePassed.Text = "lblTimePassed";

            lblTimeRemain = new Label();
            //lblTimeRemain.Size = new Size(127, 17);
            lblTimeRemain.Text = "lblTimeRemain";

            lblPassedVal = new Label();
            //lblPassedVal.Size = new Size(281, 20);
            lblPassedVal.Text = "lblPassedVal";
            lblPassedVal.TextAlignment = TextAlignment.Right;

            lblRemainVal = new Label();
            //lblRemainVal.Size = new Size(281, 20);
            lblRemainVal.Text = "lblRemainVal";
            lblRemainVal.TextAlignment = TextAlignment.Right;

            lblTimeTotal = new Label();
            //lblTimeTotal.Size = new Size(102, 17);
            lblTimeTotal.Text = "lblTimeTotal";
            lblTimeTotal.TextAlignment = TextAlignment.Right;

            lblTotalVal = new Label();
            //lblTotalVal.Size = new Size(281, 20);
            lblTotalVal.Text = "lblTotalVal";

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
                        //ScaleHeight = true,
                        Cells = { timesPanel }
                    },
                    null
                }
            };

            ClientSize = new Size(531, 152);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "ProgressDlg";
            Topmost = true;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f, FontStyle.Bold);
            ResumeLayout();
        }
    }
}
