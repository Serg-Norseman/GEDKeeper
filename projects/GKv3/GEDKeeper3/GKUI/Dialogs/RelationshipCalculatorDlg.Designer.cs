using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class RelationshipCalculatorDlg
    {
        private Label lblKinship;
        private TextArea txtResult;
        private Button btnRec2Select;
        private Button btnRec1Select;
        private TextBox Edit2;
        private TextBox Edit1;
        private Label Lab2;
        private Label Lab1;
        private Button btnClose;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnClose = new Button();
            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(91, 24);
            btnClose.Text = "btnClose";

            Lab1 = new Label();
            Lab1.Text = "XXX1";

            Lab2 = new Label();
            Lab2.Text = "XXX2";

            Edit1 = new TextBox();
            Edit1.ReadOnly = true;

            Edit2 = new TextBox();
            Edit2.ReadOnly = true;

            btnRec1Select = new Button();
            btnRec1Select.Size = new Size(77, 22);
            btnRec1Select.Text = "btnRec1Select";
            btnRec1Select.Click += btnRec1Select_Click;

            btnRec2Select = new Button();
            btnRec2Select.Size = new Size(77, 21);
            btnRec2Select.Text = "btnRec2Select";
            btnRec2Select.Click += btnRec2Select_Click;

            lblKinship = new Label();
            lblKinship.Text = "Kinship";

            txtResult = new TextArea();
            txtResult.ReadOnly = true;

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { Lab1 }
                    },
                    new TableRow {
                        Cells = { Edit1, btnRec1Select }
                    },
                    new TableRow {
                        Cells = { Lab2 }
                    },
                    new TableRow {
                        Cells = { Edit2, btnRec2Select }
                    },
                    new TableRow {
                        Cells = { lblKinship }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { txtResult }
                    },
                    new TableRow {
                        Cells = { null, btnClose }
                    }
                }
            };

            AbortButton = btnClose;
            ClientSize = new Size(399, 286);
            Title = "RelationshipCalculator";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
