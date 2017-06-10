using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    partial class ACOptionsControl
    {
        private ColorDialog ColorDialog1;
        private Label acb0;
        private Label acb1;
        private Label acb2;
        private Label acb3;
        private Label acb4;
        private Label acb5;
        private Label acb6;
        private Label acb7;
        private Label acbText;
        private Label acbBack;
        private Label acbLine;
        private CheckBox chkHideEmptySegments;

        private void InitializeComponent()
        {
            acbLine = new Label();
            acbBack = new Label();
            acbText = new Label();
            acb7 = new Label();
            acb6 = new Label();
            acb5 = new Label();
            acb4 = new Label();
            acb3 = new Label();
            acb2 = new Label();
            acb1 = new Label();
            acb0 = new Label();
            ColorDialog1 = new ColorDialog();
            chkHideEmptySegments = new CheckBox();
            SuspendLayout();

            acbLine.BorderStyle = BorderStyle.Fixed3D;
            acbLine.Cursor = Cursors.Pointer;
            acbLine.TextColor = Colors.White;
            acbLine.Location = new Point(344, 97);
            acbLine.Size = new Size(160, 28);
            acbLine.Text = "Line color";
            acbLine.MouseClick += lblColorClick;

            acbBack.BorderStyle = BorderStyle.Fixed3D;
            acbBack.Cursor = Cursors.Pointer;
            acbBack.Location = new Point(176, 97);
            acbBack.Size = new Size(160, 28);
            acbBack.Text = "Background color";
            acbBack.MouseClick += lblColorClick;

            acbText.BorderStyle = BorderStyle.Fixed3D;
            acbText.Cursor = Cursors.Pointer;
            acbText.TextColor = Colors.White;
            acbText.Location = new Point(8, 97);
            acbText.Size = new Size(160, 28);
            acbText.Text = "Text color";
            acbText.MouseClick += lblColorClick;

            acb7.BorderStyle = BorderStyle.Fixed3D;
            acb7.Cursor = Cursors.Pointer;
            acb7.Location = new Point(512, 56);
            acb7.Size = new Size(160, 28);
            acb7.Text = "Circle 7";
            acb7.MouseClick += lblColorClick;

            acb6.BorderStyle = BorderStyle.Fixed3D;
            acb6.Cursor = Cursors.Pointer;
            acb6.Location = new Point(344, 56);
            acb6.Size = new Size(160, 28);
            acb6.Text = "Circle 6";
            acb6.MouseClick += lblColorClick;

            acb5.BorderStyle = BorderStyle.Fixed3D;
            acb5.Cursor = Cursors.Pointer;
            acb5.Location = new Point(176, 56);
            acb5.Size = new Size(160, 28);
            acb5.Text = "Circle 5";
            acb5.MouseClick += lblColorClick;

            acb4.BorderStyle = BorderStyle.Fixed3D;
            acb4.Cursor = Cursors.Pointer;
            acb4.Location = new Point(8, 56);
            acb4.Size = new Size(160, 28);
            acb4.Text = "Circle 4";
            acb4.MouseClick += lblColorClick;

            acb3.BorderStyle = BorderStyle.Fixed3D;
            acb3.Cursor = Cursors.Pointer;
            acb3.Location = new Point(512, 15);
            acb3.Size = new Size(160, 28);
            acb3.Text = "Circle 3";
            acb3.MouseClick += lblColorClick;

            acb2.BorderStyle = BorderStyle.Fixed3D;
            acb2.Cursor = Cursors.Pointer;
            acb2.Location = new Point(344, 15);
            acb2.Size = new Size(160, 28);
            acb2.Text = "Circle 2";
            acb2.MouseClick += lblColorClick;

            acb1.BorderStyle = BorderStyle.Fixed3D;
            acb1.Cursor = Cursors.Pointer;
            acb1.Location = new Point(176, 15);
            acb1.Size = new Size(160, 28);
            acb1.Text = "Circle 1";
            acb1.MouseClick += lblColorClick;

            acb0.BorderStyle = BorderStyle.Fixed3D;
            acb0.Cursor = Cursors.Pointer;
            acb0.Location = new Point(8, 15);
            acb0.Size = new Size(160, 28);
            acb0.Text = "Circle 0";
            acb0.MouseClick += lblColorClick;

            chkHideEmptySegments.Location = new Point(8, 142);
            chkHideEmptySegments.Size = new Size(328, 24);
            chkHideEmptySegments.Text = "chkHideEmptySegments";

            Controls.Add(chkHideEmptySegments);
            Controls.Add(acb0);
            Controls.Add(acb1);
            Controls.Add(acb2);
            Controls.Add(acb3);
            Controls.Add(acb4);
            Controls.Add(acb5);
            Controls.Add(acb6);
            Controls.Add(acb7);
            Controls.Add(acbText);
            Controls.Add(acbBack);
            Controls.Add(acbLine);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Size = new Size(690, 183);
            ResumeLayout();
        }
    }
}
