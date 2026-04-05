#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class EventDefEditDlg
    {
        private Label lblName;
        private TextField txtName;
        private Label lblTag;
        private ComboBox cmbTag;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblType;
        private TextField txtType;
        private CheckBox chkEnabled;
        private Label lblDesc;
        private TextField txtDesc;

        private void InitializeComponent()
        {
            lblName = new Label();
            txtName = new TextField();
            lblTag = new Label();
            cmbTag = new ComboBox();
            btnAccept = new Button();
            btnCancel = new Button();
            lblType = new Label();
            txtType = new TextField();
            chkEnabled = new CheckBox();
            lblDesc = new Label();
            txtDesc = new TextField();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 9;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 10;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblName.Location = new Point(1, 1);
            lblName.TabIndex = 0;

            txtName.Location = new Point(16, 1);
            txtName.Size = new Size(27, 1);
            txtName.TabIndex = 1;
            txtName.KeyPress += edName_KeyPress;

            lblTag.Location = new Point(1, 3);
            lblTag.TabIndex = 2;

            cmbTag.Location = new Point(16, 3);
            cmbTag.Size = new Size(27, 2);
            cmbTag.TabIndex = 3;

            lblType.Location = new Point(1, 5);
            lblType.TabIndex = 4;

            txtType.Location = new Point(16, 5);
            txtType.Size = new Size(27, 1);
            txtType.TabIndex = 5;

            chkEnabled.Location = new Point(1, 7);
            chkEnabled.TabIndex = 6;

            lblDesc.Location = new Point(1, 9);
            lblDesc.TabIndex = 7;

            txtDesc.Location = new Point(16, 9);
            txtDesc.Size = new Size(27, 1);
            txtDesc.TabIndex = 8;

            Size = new Size(46, 14);
            Add(lblDesc);
            Add(txtDesc);
            Add(chkEnabled);
            Add(lblType);
            Add(txtType);
            Add(lblName);
            Add(txtName);
            Add(lblTag);
            Add(cmbTag);
        }
    }
}
