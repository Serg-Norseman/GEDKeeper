#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class PersonalNameEditDlg
    {
        private TextField txtMarriedSurname;
        private Label lblMarriedSurname;
        private ComboBox cmbNameType;
        private Label lblType;
        private TextField txtNickname;
        private TextField txtNameSuffix;
        private TextField txtNamePrefix;
        private TextField txtSurnamePrefix;
        private TextField txtPatronymic;
        private TextField txtName;
        private TextField txtSurname;
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
            lblSurname = new Label();
            btnAccept = new Button();
            lblName = new Label();
            lblPatronymic = new Label();
            btnCancel = new Button();
            lblSurnamePrefix = new Label();
            lblNamePrefix = new Label();
            lblNameSuffix = new Label();
            lblNickname = new Label();
            txtSurname = new TextField();
            txtName = new TextField();
            txtPatronymic = new TextField();
            txtSurnamePrefix = new TextField();
            txtNamePrefix = new TextField();
            txtNameSuffix = new TextField();
            txtNickname = new TextField();
            lblType = new Label();
            cmbNameType = new ComboBox();
            lblMarriedSurname = new Label();
            txtMarriedSurname = new TextField();
            cmbLanguage = new ComboBox();
            lblLanguage = new Label();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 22;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 23;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblSurname.Location = new Point(1, 1);
            lblSurname.TabIndex = 0;

            txtSurname.Location = new Point(1, 2);
            txtSurname.Size = new Size(30, 1);
            txtSurname.TabIndex = 1;
            txtSurname.KeyDown += txtXName_KeyDown;
            txtSurname.Leave += txtXName_Leave;

            lblMarriedSurname.Location = new Point(1, 4);
            lblMarriedSurname.TabIndex = 2;

            txtMarriedSurname.Location = new Point(1, 5);
            txtMarriedSurname.Size = new Size(30, 1);
            txtMarriedSurname.TabIndex = 3;
            txtMarriedSurname.KeyDown += txtXName_KeyDown;
            txtMarriedSurname.Leave += txtXName_Leave;

            lblName.Location = new Point(1, 7);
            lblName.TabIndex = 4;

            txtName.Location = new Point(1, 8);
            txtName.Size = new Size(30, 1);
            txtName.TabIndex = 5;
            txtName.KeyDown += txtXName_KeyDown;
            txtName.Leave += txtXName_Leave;

            lblPatronymic.Location = new Point(1, 10);
            lblPatronymic.TabIndex = 6;

            txtPatronymic.Location = new Point(1, 11);
            txtPatronymic.Size = new Size(30, 1);
            txtPatronymic.TabIndex = 7;
            txtPatronymic.KeyDown += txtXName_KeyDown;
            txtPatronymic.Leave += txtXName_Leave;

            lblSurnamePrefix.Location = new Point(32, 1);
            lblSurnamePrefix.TabIndex = 8;

            txtSurnamePrefix.Location = new Point(32, 2);
            txtSurnamePrefix.Size = new Size(30, 1);
            txtSurnamePrefix.TabIndex = 9;

            lblNamePrefix.Location = new Point(32, 4);
            lblNamePrefix.TabIndex = 10;

            txtNamePrefix.Location = new Point(32, 5);
            txtNamePrefix.Size = new Size(30, 1);
            txtNamePrefix.TabIndex = 11;

            lblNameSuffix.Location = new Point(32, 7);
            lblNameSuffix.TabIndex = 12;

            txtNameSuffix.Location = new Point(32, 8);
            txtNameSuffix.Size = new Size(30, 1);
            txtNameSuffix.TabIndex = 13;

            lblNickname.Location = new Point(32, 10);
            lblNickname.TabIndex = 14;

            txtNickname.Location = new Point(32, 11);
            txtNickname.Size = new Size(30, 1);
            txtNickname.TabIndex = 15;

            lblType.Location = new Point(1, 13);
            lblType.TabIndex = 16;

            cmbNameType.Location = new Point(1, 14);
            cmbNameType.Size = new Size(30, 2);
            cmbNameType.TabIndex = 17;

            lblLanguage.Location = new Point(1, 16);
            lblLanguage.TabIndex = 20;

            cmbLanguage.Location = new Point(1, 17);
            cmbLanguage.Size = new Size(30, 2);
            cmbLanguage.TabIndex = 21;
            cmbLanguage.SelectedIndexChanged += cmbLanguage_SelectedIndexChanged;

            Size = new Size(64, 23);
            Add(cmbLanguage);
            Add(lblLanguage);
            Add(lblMarriedSurname);
            Add(txtMarriedSurname);
            Add(lblType);
            Add(cmbNameType);
            Add(lblSurname);
            Add(lblName);
            Add(lblPatronymic);
            Add(lblSurnamePrefix);
            Add(lblNamePrefix);
            Add(lblNameSuffix);
            Add(lblNickname);
            Add(txtSurname);
            Add(txtName);
            Add(txtPatronymic);
            Add(txtSurnamePrefix);
            Add(txtNamePrefix);
            Add(txtNameSuffix);
            Add(txtNickname);
        }
    }
}
