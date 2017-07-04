using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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
            btnPersonAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");

            cmbRelation = new ComboBox();
            //cmbRelation.Sorted = true;

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
                    }
                }
            };

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;
            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += CancelClickHandler;
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
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
