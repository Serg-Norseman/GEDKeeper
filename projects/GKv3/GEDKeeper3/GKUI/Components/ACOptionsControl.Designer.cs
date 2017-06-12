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

            //acbLine.BorderStyle = BorderStyle.Fixed3D;
            acbLine.Cursor = Cursors.Pointer;
            acbLine.TextColor = Colors.White;
            acbLine.Size = new Size(160, 28);
            acbLine.Text = "Line color";
            acbLine.MouseDown += lblColorClick;

            //acbBack.BorderStyle = BorderStyle.Fixed3D;
            acbBack.Cursor = Cursors.Pointer;
            acbBack.Size = new Size(160, 28);
            acbBack.Text = "Background color";
            acbBack.MouseDown += lblColorClick;

            //acbText.BorderStyle = BorderStyle.Fixed3D;
            acbText.Cursor = Cursors.Pointer;
            acbText.TextColor = Colors.White;
            acbText.Size = new Size(160, 28);
            acbText.Text = "Text color";
            acbText.MouseDown += lblColorClick;

            //acb7.BorderStyle = BorderStyle.Fixed3D;
            acb7.Cursor = Cursors.Pointer;
            acb7.Size = new Size(160, 28);
            acb7.Text = "Circle 7";
            acb7.MouseDown += lblColorClick;

            //acb6.BorderStyle = BorderStyle.Fixed3D;
            acb6.Cursor = Cursors.Pointer;
            acb6.Size = new Size(160, 28);
            acb6.Text = "Circle 6";
            acb6.MouseDown += lblColorClick;

            //acb5.BorderStyle = BorderStyle.Fixed3D;
            acb5.Cursor = Cursors.Pointer;
            acb5.Size = new Size(160, 28);
            acb5.Text = "Circle 5";
            acb5.MouseDown += lblColorClick;

            //acb4.BorderStyle = BorderStyle.Fixed3D;
            acb4.Cursor = Cursors.Pointer;
            acb4.Size = new Size(160, 28);
            acb4.Text = "Circle 4";
            acb4.MouseDown += lblColorClick;

            //acb3.BorderStyle = BorderStyle.Fixed3D;
            acb3.Cursor = Cursors.Pointer;
            acb3.Size = new Size(160, 28);
            acb3.Text = "Circle 3";
            acb3.MouseDown += lblColorClick;

            //acb2.BorderStyle = BorderStyle.Fixed3D;
            acb2.Cursor = Cursors.Pointer;
            acb2.Size = new Size(160, 28);
            acb2.Text = "Circle 2";
            acb2.MouseDown += lblColorClick;

            //acb1.BorderStyle = BorderStyle.Fixed3D;
            acb1.Cursor = Cursors.Pointer;
            acb1.Size = new Size(160, 28);
            acb1.Text = "Circle 1";
            acb1.MouseDown += lblColorClick;

            //acb0.BorderStyle = BorderStyle.Fixed3D;
            acb0.Cursor = Cursors.Pointer;
            acb0.Size = new Size(160, 28);
            acb0.Text = "Circle 0";
            acb0.MouseDown += lblColorClick;

            chkHideEmptySegments.Size = new Size(328, 24);
            chkHideEmptySegments.Text = "chkHideEmptySegments";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { acb0, acb1, acb2, acb3 }
                    },
                    new TableRow {
                        Cells = { acb4, acb5, acb6, acb7 }
                    },
                    new TableRow {
                        Cells = { acbText, acbBack, acbLine }
                    },
                    new TableRow {
                        Cells = { chkHideEmptySegments }
                    },
                    null
                }
            };

            Size = new Size(690, 183);
            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
