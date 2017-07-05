using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class DayTipsDlg
    {
        private Scrollable Shape1;
        private CheckBox chkShow;
        private Button btnNextTip;
        private Button btnClose;
        private Scrollable Shape2;
        private Scrollable Shape3;
        private Label lblTitle;
        private Eto.Forms.ImageView Image1;
        private Label txtTip;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblTitle = new Label();
            lblTitle.Text = "lblTitle";
            lblTitle.TextAlignment = TextAlignment.Left;
            lblTitle.VerticalAlignment = VerticalAlignment.Center;

            Shape1 = new Scrollable();
            Shape1.BackgroundColor = Colors.White;
            Shape1.Border = BorderType.Line;
            Shape1.Height = 50;
            Shape1.Content = new HDefStackLayout() { Items = { lblTitle } };

            Image1 = new Eto.Forms.ImageView();
            Image1.Size = new Size(41, 43);
            Image1.Image = Bitmap.FromResource("Resources.image_tips_light.png");

            var imgLayout = new VDefStackLayout();
            imgLayout.HorizontalContentAlignment = HorizontalAlignment.Center;
            imgLayout.VerticalContentAlignment = VerticalAlignment.Top;
            imgLayout.Items.Add(Image1);

            Shape2 = new Scrollable();
            Shape2.BackgroundColor = Colors.Gray;
            Shape2.Border = BorderType.Line;
            Shape2.Width = 100;
            Shape2.Content = imgLayout;

            txtTip = new Label();
            txtTip.Wrap = WrapMode.Word;
            txtTip.TextAlignment = TextAlignment.Left;
            txtTip.VerticalAlignment = VerticalAlignment.Top;

            Shape3 = new Scrollable();
            Shape3.BackgroundColor = Colors.White;
            Shape3.Border = BorderType.Line;
            Shape3.Height = 204;
            Shape3.Content = new DefTableLayout() { Rows = { new TableRow(txtTip) { ScaleHeight = true } } };

            var panel1 = new DynamicLayout();
            panel1.BeginHorizontal();
            panel1.Add(Shape2);
            panel1.BeginVertical();
            panel1.Add(Shape1);
            panel1.Add(Shape3);
            panel1.EndVertical();
            panel1.EndHorizontal();

            chkShow = new CheckBox();
            chkShow.Checked = true;
            chkShow.Text = "chkShow";

            btnNextTip = new Button();
            btnNextTip.Size = new Size(130, 26);
            btnNextTip.Text = "btnNextTip";
            btnNextTip.Click += btnNextTip_Click;

            btnClose = new Button();
            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(130, 26);
            btnClose.Text = "btnClose";
            btnClose.Image = Bitmap.FromResource("Resources.btn_cancel.gif");
            btnClose.Click += CancelClickHandler;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panel1 }
                    },
                    UIHelper.MakeDialogFooter(chkShow, null, btnNextTip, btnClose)
                }
            };

            AbortButton = btnClose;
            Title = " ";
            Topmost = true;

            SetPredefProperties(550, 310);
            lblTitle.Font = new Font("Arial", 16F, FontStyle.Bold);

            ResumeLayout();
        }
    }
}
