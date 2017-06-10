using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class DayTipsDlg
    {
        private Panel Shape1;
        private CheckBox chkShow;
        private Button btnNextTip;
        private Button btnClose;
        private Panel Shape2;
        private Panel Shape3;
        private Label lblTitle;
        private ImageView Image1;
        private TextArea txtTip;

        private void InitializeComponent()
        {
            Shape1 = new Panel();
            chkShow = new CheckBox();
            btnNextTip = new Button();
            btnClose = new Button();
            Shape2 = new Panel();
            Shape3 = new Panel();
            lblTitle = new Label();
            Image1 = new ImageView();
            txtTip = new TextArea();
            SuspendLayout();

            Shape1.BackgroundColor = Colors.White;
            Shape1.BorderStyle = BorderStyle.FixedSingle;
            Shape1.TextColor = Colors.Black;
            Shape1.Location = new Point(123, 10);
            Shape1.Size = new Size(405, 48);

            chkShow.Checked = true;
            chkShow.Location = new Point(25, 267);
            chkShow.Size = new Size(234, 21);
            chkShow.Text = "chkShow";

            btnNextTip.Location = new Point(302, 262);
            btnNextTip.Size = new Size(105, 31);
            btnNextTip.Text = "btnNextTip";
            btnNextTip.Click += btnNextTip_Click;

            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Location = new Point(414, 262);
            btnClose.Size = new Size(105, 31);
            btnClose.Text = "btnClose";

            Shape2.BackgroundColor = Colors.Gray;
            Shape2.BorderStyle = BorderStyle.FixedSingle;
            Shape2.Location = new Point(22, 10);
            Shape2.Size = new Size(103, 224);

            Shape3.BackgroundColor = Colors.White;
            Shape3.BorderStyle = BorderStyle.FixedSingle;
            Shape3.TextColor = Colors.Black;
            Shape3.Location = new Point(123, 57);
            Shape3.Size = new Size(405, 177);

            lblTitle.BackgroundColor = Colors.White;
            lblTitle.Font = new Font("Arial", 16F, FontStyle.Bold);
            lblTitle.Location = new Point(134, 19);
            lblTitle.Size = new Size(378, 36);
            lblTitle.Text = "lblTitle";

            Image1.Location = new Point(45, 29);
            Image1.Size = new Size(57, 52);
            Image1.SizeMode = PictureBoxSizeMode.StretchImage;

            txtTip.BorderStyle = BorderStyle.None;
            txtTip.Location = new Point(137, 69);
            txtTip.Size = new Size(371, 154);

            AbortButton = btnClose;
            ClientSize = new Size(547, 313);
            Controls.Add(lblTitle);
            Controls.Add(Image1);
            Controls.Add(chkShow);
            Controls.Add(txtTip);
            Controls.Add(btnNextTip);
            Controls.Add(btnClose);
            Controls.Add(Shape1);
            Controls.Add(Shape3);
            Controls.Add(Shape2);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = " ";
            Topmost = true;
            ResumeLayout();
        }
    }
}
