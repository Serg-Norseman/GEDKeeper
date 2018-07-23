using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
        private ComboBox cmbLanguage;
        private Label lblLanguage;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblSurname = new Label();
            lblSurname.Text = "lblSurname";

            lblSurnamePrefix = new Label();
            lblSurnamePrefix.Text = "lblSurnamePrefix";

            txtSurname = new TextBox();
            txtSurname.Width = 200;
            txtSurname.KeyDown += edName_KeyDown;

            txtSurnamePrefix = new TextBox();
            txtSurnamePrefix.Width = 200;

            lblMarriedSurname = new Label();
            lblMarriedSurname.Text = "lblMarriedSurname";

            lblNamePrefix = new Label();
            lblNamePrefix.Text = "lblNamePrefix";

            txtMarriedSurname = new TextBox();
            txtMarriedSurname.KeyDown += edName_KeyDown;

            txtNamePrefix = new TextBox();

            lblName = new Label();
            lblName.Text = "lblName";

            lblNameSuffix = new Label();
            lblNameSuffix.Text = "lblNameSuffix";

            txtName = new TextBox();
            txtName.KeyDown += edName_KeyDown;

            txtNameSuffix = new TextBox();

            lblPatronymic = new Label();
            lblPatronymic.Text = "lblPatronymic";

            lblNickname = new Label();
            lblNickname.Text = "lblNickname";

            txtPatronymic = new TextBox();
            txtPatronymic.KeyDown += edName_KeyDown;

            txtNickname = new TextBox();

            lblType = new Label();
            lblType.Text = "lblType";

            cmbNameType = new ComboBox();
            cmbNameType.ReadOnly = true;

            lblLanguage = new Label();
            lblLanguage.Text = "lblLanguage";

            cmbLanguage = new ComboBox();
            cmbLanguage.ReadOnly = true;

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;
            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += CancelClickHandler;
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

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
                    new TableRow {
                        Cells = { lblLanguage, null }
                    },
                    new TableRow {
                        Cells = { cmbLanguage, null }
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
