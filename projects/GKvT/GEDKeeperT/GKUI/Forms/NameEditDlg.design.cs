#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class NameEditDlg
    {
        private Label lblName;
        private TextField txtName;
        private Label lblSex;
        private ComboBox cmbSex;
        private Button btnAccept;
        private Button btnCancel;
        private FrameView grpPatronymics;
        private Label lblFemale;
        private TextField txtFPatr;
        private Label lblMale;
        private TextField txtMPatr;

        private void InitializeComponent()
        {
            lblName = new Label();
            txtName = new TextField();
            lblSex = new Label();
            cmbSex = new ComboBox();
            btnAccept = new Button();
            btnCancel = new Button();
            grpPatronymics = new FrameView();
            lblFemale = new Label();
            lblMale = new Label();
            txtFPatr = new TextField();
            txtMPatr = new TextField();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 5;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 6;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblName.Location = new Point(1, 1);
            lblName.TabIndex = 0;

            txtName.Location = new Point(10, 1);
            txtName.Size = new Size(20, 1);
            txtName.TabIndex = 1;
            txtName.KeyPress += edName_KeyPress;

            lblSex.Location = new Point(1, 3);
            lblSex.TabIndex = 2;

            cmbSex.Location = new Point(10, 3);
            cmbSex.Size = new Size(16, 2);
            cmbSex.TabIndex = 3;

            grpPatronymics.Add(lblFemale);
            grpPatronymics.Add(lblMale);
            grpPatronymics.Add(txtFPatr);
            grpPatronymics.Add(txtMPatr);
            grpPatronymics.Location = new Point(1, 5);
            grpPatronymics.Size = new Size(34, 7);
            grpPatronymics.TabIndex = 4;
            grpPatronymics.TabStop = false;

            lblFemale.Location = new Point(1, 1);
            lblFemale.TabIndex = 0;

            txtFPatr.Location = new Point(10, 1);
            txtFPatr.Size = new Size(26, 1);
            txtFPatr.TabIndex = 1;
            txtFPatr.KeyPress += edName_KeyPress;

            lblMale.Location = new Point(1, 3);
            lblMale.TabIndex = 2;

            txtMPatr.Location = new Point(10, 3);
            txtMPatr.Size = new Size(26, 1);
            txtMPatr.TabIndex = 3;
            txtMPatr.KeyPress += edName_KeyPress;

            Size = new Size(38, 25);
            Add(lblName);
            Add(txtName);
            Add(lblSex);
            Add(cmbSex);
            Add(grpPatronymics);
        }
    }
}
