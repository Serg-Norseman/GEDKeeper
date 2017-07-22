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
            btnClose.Size = new Size(130, 26);
            btnClose.Text = "btnClose";
            btnClose.Click += CancelClickHandler;
            btnClose.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            Lab1 = new Label();
            Lab1.Text = "XXX1";

            Lab2 = new Label();
            Lab2.Text = "XXX2";

            Edit1 = new TextBox();
            Edit1.ReadOnly = true;
            Edit1.Width = 300;

            Edit2 = new TextBox();
            Edit2.ReadOnly = true;
            Edit2.Width = 300;

            btnRec1Select = new Button();
            btnRec1Select.Size = new Size(80, 26);
            btnRec1Select.Text = "btnRec1Select";
            btnRec1Select.Click += btnRec1Select_Click;

            btnRec2Select = new Button();
            btnRec2Select.Size = new Size(80, 26);
            btnRec2Select.Text = "btnRec2Select";
            btnRec2Select.Click += btnRec2Select_Click;

            lblKinship = new Label();
            lblKinship.Text = "Kinship";

            txtResult = new TextArea();
            txtResult.ReadOnly = true;
            txtResult.Height = 140;

            var dataPanel = new TableLayout {
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { Lab1, TableLayout.Horizontal(10, new TableCell(Edit1, true), btnRec1Select) }
                    },
                    new TableRow {
                        Cells = { Lab2, TableLayout.Horizontal(10, new TableCell(Edit2, true), btnRec2Select) }
                    }
                }
            };

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { dataPanel }
                    },
                    new TableRow {
                        Cells = { lblKinship }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { txtResult }
                    },
                    UIHelper.MakeDialogFooter(null, btnClose)
                }
            };

            AbortButton = btnClose;
            Title = "RelationshipCalculator";

            SetPredefProperties(400, 280);
            ResumeLayout();
        }
    }
}
