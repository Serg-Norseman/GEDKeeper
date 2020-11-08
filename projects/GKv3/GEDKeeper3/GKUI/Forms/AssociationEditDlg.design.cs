using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class AssociationEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblRelation;
        private ComboBox cmbRelation;
        private Label lblPerson;
        private TextBox txtPerson;
        private Button btnPersonAdd;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblRelation = new Label();
            lblRelation.Text = "lblRelation";

            lblPerson = new Label();
            lblPerson.Text = "lblPerson";

            btnPersonAdd = new Button();
            btnPersonAdd.Size = new Size(26, 26);
            btnPersonAdd.Click += btnPersonAdd_Click;

            cmbRelation = new ComboBox();

            txtPerson = new TextBox();
            txtPerson.ReadOnly = true;
            txtPerson.Width = 280;

            var panelData = new TableLayout {
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblRelation, cmbRelation }
                    },
                    new TableRow {
                        Cells = { lblPerson, TableLayout.Horizontal(10, new TableCell(txtPerson, true), btnPersonAdd) }
                    },
                    null
                }
            };

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panelData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "AssociationEditDlg";

            SetPredefProperties(500, 180);
            ResumeLayout();
        }
    }
}
