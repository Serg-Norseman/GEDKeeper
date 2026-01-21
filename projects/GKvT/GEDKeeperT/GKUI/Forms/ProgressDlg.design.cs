using Terminal.Gui;

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
            lblTitle = new Label() { X = 1, Y = 0 };
            lblTitle.Text = "test";

            ProgressBar1 = new ProgressBar() { X = 1, Y = 1, Width = 60, Height = 1, ProgressBarStyle = ProgressBarStyle.MarqueeContinuous };
            ProgressBar1.Fraction = 1.0f;

            lblTimePassed = new Label() { X = 1, Y = 3 };

            lblPassedVal = new Label() { X = 53, Y = 3 };

            lblTimeRemain = new Label() { X = 1, Y = 4 };

            lblRemainVal = new Label() { X = 53, Y = 4 };

            lblTimeTotal = new Label() { X = 1, Y = 5 };

            lblTotalVal = new Label() { X = 53, Y = 5 };

            btnCancel = new Button() { Enabled = false, TabIndex = 1 };
            btnCancel.MouseClick += btnCancel_Click;

            //AutoSize = true;
            Width = 64;
            Height = 10;

            Add(ProgressBar1);
            Add(lblTitle);
            Add(lblTimePassed);
            Add(lblTimeRemain);
            Add(lblPassedVal);
            Add(lblRemainVal);
            Add(lblTimeTotal);
            Add(lblTotalVal);
            AddButton(btnCancel);
        }
    }
}
