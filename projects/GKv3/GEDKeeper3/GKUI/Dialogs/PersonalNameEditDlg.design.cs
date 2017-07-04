using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class PersonalNameEditDlg
    {
        private TextBox txtMarriedSurname;
        private Label lblMarriedSurname;
        private ComboBox cmbNameType;
        private Label lblType;
        private TextBox txtNickname;
        private TextBox txtNameSuffix;
        private TextBox txtNamePrefix;
        private TextBox txtSurnamePrefix;
        private TextBox txtPatronymic;
        private TextBox txtName;
        private TextBox txtSurname;
        private Label lblNickname;
        private Label lblNameSuffix;
        private Label lblNamePrefix;
        private Label lblSurnamePrefix;
        private Button btnCancel;
        private Label lblPatronymic;
        private Label lblName;
        private Button btnAccept;
        private Label lblSurname;

        private void InitializeComponent()
        {
            SuspendLayout();

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

            lblSurname = new Label();
            lblSurname.Text = "lblSurname";

            lblSurnamePrefix = new Label();
            lblSurnamePrefix.Text = "lblSurnamePrefix";

            txtSurname = new TextBox();
            //txtSurname.Size = new Size(226, 24);

            txtSurnamePrefix = new TextBox();
            //txtSurnamePrefix.Size = new Size(169, 24);

            lblMarriedSurname = new Label();
            lblMarriedSurname.Text = "lblMarriedSurname";

            lblNamePrefix = new Label();
            lblNamePrefix.Text = "lblNamePrefix";

            txtMarriedSurname = new TextBox();
            //txtMarriedSurname.Size = new Size(226, 24);

            txtNamePrefix = new TextBox();
            //txtNamePrefix.Size = new Size(169, 24);

            lblName = new Label();
            lblName.Text = "lblName";

            lblNameSuffix = new Label();
            lblNameSuffix.Text = "lblNameSuffix";

            txtName = new TextBox();
            //txtName.Size = new Size(226, 24);

            txtNameSuffix = new TextBox();
            //txtNameSuffix.Size = new Size(169, 24);

            lblPatronymic = new Label();
            lblPatronymic.Text = "lblPatronymic";

            lblNickname = new Label();
            lblNickname.Text = "lblNickname";

            txtPatronymic = new TextBox();
            //txtPatronymic.Size = new Size(226, 24);

            txtNickname = new TextBox();
            //txtNickname.Size = new Size(169, 24);

            lblType = new Label();
            lblType.Text = "lblType";

            cmbNameType = new ComboBox();
            cmbNameType.ReadOnly = true;
            //cmbNameType.Size = new Size(225, 25);

            var panel = new TableLayout {
                Padding = new Padding(0),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblSurname, lblSurnamePrefix }
                    },
                    new TableRow {
                        Cells = { txtSurname, txtSurnamePrefix }
                    },
                    new TableRow {
                        Cells = { lblMarriedSurname, lblNamePrefix }
                    },
                    new TableRow {
                        Cells = { txtMarriedSurname, txtNamePrefix }
                    },
                    new TableRow {
                        Cells = { lblName, lblNameSuffix }
                    },
                    new TableRow {
                        Cells = { txtName, txtNameSuffix }
                    },
                    new TableRow {
                        Cells = { lblPatronymic, lblNickname }
                    },
                    new TableRow {
                        Cells = { txtPatronymic, txtNickname }
                    },
                    new TableRow {
                        Cells = { lblType, null }
                    },
                    new TableRow {
                        //ScaleHeight = true,
                        Cells = { cmbNameType, null }
                    },
                    null
                }
            };

            Content = new DefTableLayout {
                Rows = {
                    panel,
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "PersonalNameEditDlg";

            SetPredefProperties(440, 360);
            ResumeLayout();
        }
    }
}
