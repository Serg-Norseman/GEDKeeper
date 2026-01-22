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
            Add(lblTitle);

            ProgressBar1 = new ProgressBar() { X = 1, Y = 1, Width = 60, Height = 1, ProgressBarStyle = ProgressBarStyle.MarqueeContinuous, Fraction = 1.0f };
            Add(ProgressBar1);

            lblTimePassed = new Label() { X = 1, Y = 3 };
            Add(lblTimePassed);

            lblPassedVal = new Label() { X = 53, Y = 3 };
            Add(lblPassedVal);

            lblTimeRemain = new Label() { X = 1, Y = 4 };
            Add(lblTimeRemain);

            lblRemainVal = new Label() { X = 53, Y = 4 };
            Add(lblRemainVal);

            lblTimeTotal = new Label() { X = 1, Y = 5 };
            Add(lblTimeTotal);

            lblTotalVal = new Label() { X = 53, Y = 5 };
            Add(lblTotalVal);

            btnCancel = new Button() { Enabled = false, TabIndex = 1 };
            btnCancel.MouseClick += btnCancel_Click;
            AddButton(btnCancel);

            Width = 64;
            Height = 10;
        }
    }
}
