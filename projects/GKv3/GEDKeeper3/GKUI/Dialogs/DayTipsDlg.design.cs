using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

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
        private Eto.Forms.ImageView Image1;
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
            Image1 = new Eto.Forms.ImageView();
            txtTip = new TextArea();

            SuspendLayout();

            Shape1.BackgroundColor = Colors.White;
            //Shape1.BorderStyle = BorderStyle.FixedSingle;
            //Shape1.TextColor = Colors.Black;
            Shape1.Size = new Size(405, 48);

            chkShow.Checked = true;
            chkShow.Size = new Size(234, 21);
            chkShow.Text = "chkShow";

            btnNextTip.Size = new Size(105, 31);
            btnNextTip.Text = "btnNextTip";
            btnNextTip.Click += btnNextTip_Click;

            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(105, 31);
            btnClose.Text = "btnClose";

            Shape2.BackgroundColor = Colors.Gray;
            //Shape2.BorderStyle = BorderStyle.FixedSingle;
            Shape2.Size = new Size(103, 224);

            Shape3.BackgroundColor = Colors.White;
            //Shape3.BorderStyle = BorderStyle.FixedSingle;
            //Shape3.TextColor = Colors.Black;
            Shape3.Size = new Size(405, 177);

            lblTitle.BackgroundColor = Colors.White;
            lblTitle.Font = new Font("Arial", 16F, FontStyle.Bold);
            lblTitle.Size = new Size(378, 36);
            lblTitle.Text = "lblTitle";

            Image1.Size = new Size(57, 52);
            //Image1.SizeMode = PictureBoxSizeMode.StretchImage;

            //txtTip.BorderStyle = BorderStyle.None;
            txtTip.Size = new Size(371, 154);

            var layout = new PixelLayout();
            layout.Add(lblTitle, 134, 19);
            layout.Add(Image1, 45, 29);
            layout.Add(chkShow, 25, 267);
            layout.Add(txtTip, 137, 69);
            layout.Add(btnNextTip, 302, 262);
            layout.Add(btnClose, 414, 262);
            layout.Add(Shape1, 123, 10);
            layout.Add(Shape3, 123, 57);
            layout.Add(Shape2, 22, 10);

            AbortButton = btnClose;
            Title = " ";
            Topmost = true;

            SetPredefProperties(550, 310);
            ResumeLayout();
        }
    }
}
