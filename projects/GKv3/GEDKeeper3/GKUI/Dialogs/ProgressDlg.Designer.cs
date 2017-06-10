using System;
using Eto.Drawing;
using Eto.Forms;

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
            ProgressBar1 = new ProgressBar();
            lblTitle = new Label();
            lblTimePassed = new Label();
            lblTimeRemain = new Label();
            lblPassedVal = new Label();
            lblRemainVal = new Label();
            lblTimeTotal = new Label();
            lblTotalVal = new Label();
            SuspendLayout();

            ProgressBar1.Location = new Point(16, 37);
            ProgressBar1.Size = new Size(497, 20);
            ProgressBar1.Step = 1;
            ProgressBar1.Style = ProgressBarStyle.Continuous;

            lblTitle.Location = new Point(16, 16);
            lblTitle.Size = new Size(47, 17);
            lblTitle.Text = "lblTitle";

            lblTimePassed.Location = new Point(16, 69);
            lblTimePassed.Size = new Size(117, 17);
            lblTimePassed.Text = "lblTimePassed";

            lblTimeRemain.Location = new Point(16, 94);
            lblTimeRemain.Size = new Size(127, 17);
            lblTimeRemain.Text = "lblTimeRemain";

            lblPassedVal.Font = new Font("Tahoma", 8.25F, FontStyle.Bold);
            lblPassedVal.Location = new Point(232, 69);
            lblPassedVal.Size = new Size(281, 20);
            lblPassedVal.Text = "lblPassedVal";

            lblRemainVal.Font = new Font("Tahoma", 8.25F, FontStyle.Bold);
            lblRemainVal.Location = new Point(232, 94);
            lblRemainVal.Size = new Size(281, 20);
            lblRemainVal.Text = "lblRemainVal";

            lblTimeTotal.Location = new Point(16, 119);
            lblTimeTotal.Size = new Size(102, 17);
            lblTimeTotal.Text = "lblTimeTotal";

            lblTotalVal.Font = new Font("Tahoma", 8.25F, FontStyle.Bold);
            lblTotalVal.Location = new Point(232, 119);
            lblTotalVal.Size = new Size(281, 20);
            lblTotalVal.Text = "lblTotalVal";

            ClientSize = new Size(531, 152);
            Controls.Add(ProgressBar1);
            Controls.Add(lblTitle);
            Controls.Add(lblTimePassed);
            Controls.Add(lblTimeRemain);
            Controls.Add(lblPassedVal);
            Controls.Add(lblRemainVal);
            Controls.Add(lblTimeTotal);
            Controls.Add(lblTotalVal);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            Padding = new Padding(8);
            ShowInTaskbar = false;
            Title = "ProgressDlg";
            Topmost = true;
            ResumeLayout();
        }
    }
}
