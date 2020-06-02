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

            txtSurname = new TextBox();
            txtSurname.Size = new Size(180, 22);
            txtSurname.KeyDown += edName_KeyDown;
            txtSurname.LostFocus += txtXName_Leave;

            lblSurnamePrefix = new Label();
            lblSurnamePrefix.Text = "lblSurnamePrefix";

            txtSurnamePrefix = new TextBox();
            txtSurnamePrefix.Size = new Size(180, 22);
            txtSurnamePrefix.KeyDown += edName_KeyDown;

            lblMarriedSurname = new Label();
            lblMarriedSurname.Text = "lblMarriedSurname";

            txtMarriedSurname = new TextBox();
            txtMarriedSurname.Size = new Size(180, 22);
            txtMarriedSurname.KeyDown += edName_KeyDown;
            txtMarriedSurname.LostFocus += txtXName_Leave;

            lblNamePrefix = new Label();
            lblNamePrefix.Text = "lblNamePrefix";

            txtNamePrefix = new TextBox();
            txtNamePrefix.Size = new Size(180, 22);
            txtNamePrefix.KeyDown += edName_KeyDown;

            lblName = new Label();
            lblName.Text = "lblName";

            txtName = new TextBox();
            txtName.Size = new Size(180, 22);
            txtName.KeyDown += edName_KeyDown;
            txtName.LostFocus += txtXName_Leave;

            lblNameSuffix = new Label();
            lblNameSuffix.Text = "lblNameSuffix";

            txtNameSuffix = new TextBox();
            txtNameSuffix.Size = new Size(180, 22);
            txtNameSuffix.KeyDown += edName_KeyDown;

            lblPatronymic = new Label();
            lblPatronymic.Text = "lblPatronymic";

            txtPatronymic = new TextBox();
            txtPatronymic.Size = new Size(180, 22);
            txtPatronymic.KeyDown += edName_KeyDown;
            txtPatronymic.LostFocus += txtXName_Leave;

            lblNickname = new Label();
            lblNickname.Text = "lblNickname";

            txtNickname = new TextBox();
            txtNickname.Size = new Size(180, 22);
            txtNickname.KeyDown += edName_KeyDown;

            lblType = new Label();
            lblType.Text = "lblType";

            cmbNameType = new ComboBox();
            cmbNameType.Size = new Size(180, 22);
            cmbNameType.ReadOnly = true;

            lblLanguage = new Label();
            lblLanguage.Text = "lblLanguage";

            cmbLanguage = new ComboBox();
            cmbLanguage.Size = new Size(180, 22);
            cmbLanguage.ReadOnly = true;

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = UIHelper.LongButtonSize;
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = UIHelper.LongButtonSize;
            btnCancel.Text = "btnCancel";
            btnCancel.Click += CancelClickHandler;

            var panel = new TableLayout {
                Padding = new Padding(0),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { new DefStackLayout(lblSurname, txtSurname),
                                  new DefStackLayout(lblSurnamePrefix, txtSurnamePrefix) }
                    },
                    new TableRow {
                        Cells = { new DefStackLayout(lblMarriedSurname, txtMarriedSurname),
                                  new DefStackLayout(lblNamePrefix, txtNamePrefix) }
                    },
                    new TableRow {
                        Cells = { new DefStackLayout(lblName, txtName),
                                  new DefStackLayout(lblNameSuffix, txtNameSuffix) }
                    },
                    new TableRow {
                        Cells = { new DefStackLayout(lblPatronymic, txtPatronymic),
                                  new DefStackLayout(lblNickname, txtNickname) }
                    },
                    new TableRow {
                        Cells = { new DefStackLayout(lblType, cmbNameType), null }
                    },
                    new TableRow {
                        Cells = { new DefStackLayout(lblLanguage, cmbLanguage), null }
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
