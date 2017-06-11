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
            lblSurname = new Label();
            btnAccept = new Button();
            lblName = new Label();
            lblPatronymic = new Label();
            btnCancel = new Button();
            lblSurnamePrefix = new Label();
            lblNamePrefix = new Label();
            lblNameSuffix = new Label();
            lblNickname = new Label();
            txtSurname = new TextBox();
            txtName = new TextBox();
            txtPatronymic = new TextBox();
            txtSurnamePrefix = new TextBox();
            txtNamePrefix = new TextBox();
            txtNameSuffix = new TextBox();
            txtNickname = new TextBox();
            lblType = new Label();
            cmbNameType = new ComboBox();
            lblMarriedSurname = new Label();
            txtMarriedSurname = new TextBox();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";

            lblSurname.Text = "lblSurname";

            lblSurnamePrefix.Text = "lblSurnamePrefix";

            txtSurname.Size = new Size(226, 24);

            txtSurnamePrefix.Size = new Size(169, 24);

            lblMarriedSurname.Text = "lblMarriedSurname";

            lblNamePrefix.Text = "lblNamePrefix";

            txtMarriedSurname.Size = new Size(226, 24);

            txtNamePrefix.Size = new Size(169, 24);

            lblName.Text = "lblName";

            lblNameSuffix.Text = "lblNameSuffix";

            txtName.Size = new Size(226, 24);

            txtNameSuffix.Size = new Size(169, 24);

            lblPatronymic.Text = "lblPatronymic";

            lblNickname.Text = "lblNickname";

            txtPatronymic.Size = new Size(226, 24);

            txtNickname.Size = new Size(169, 24);

            lblType.Text = "lblType";

            cmbNameType.ReadOnly = true;
            cmbNameType.Size = new Size(225, 25);

            Content = new TableLayout {
                Padding = new Padding(10),
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
                        Cells = { lblType }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { cmbNameType }
                    },
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(444, 323);
            Title = "PersonalNameEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
