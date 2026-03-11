#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class ParentsEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private FrameView GroupBox1;
        private Label lblChildName;
        private TextField txtChildName;
        private Label lblParents;
        private TextField txtFather;
        private TextField txtMother;
        private Button btnParentsEdit;
        private Button btnFatherAdd;
        private Button btnFatherDelete;
        private Button btnMotherAdd;
        private Button btnMotherDelete;
        private Label lblLinkageType;
        private ComboBox cmbLinkageType;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            GroupBox1 = new FrameView();
            lblChildName = new Label();
            txtChildName = new TextField();
            lblLinkageType = new Label();
            cmbLinkageType = new ComboBox();
            txtMother = new TextField();
            lblParents = new Label();
            btnParentsEdit = new Button();
            btnFatherAdd = new Button();
            btnFatherDelete = new Button();
            btnMotherAdd = new Button();
            btnMotherDelete = new Button();
            txtFather = new TextField();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 5;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 6;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblChildName.Location = new Point(1, 1);
            lblChildName.TabIndex = 0;

            txtChildName.Location = new Point(16, 1);
            txtChildName.ReadOnly = true;
            txtChildName.Size = new Size(60, 1);
            txtChildName.TabIndex = 1;

            lblLinkageType.Location = new Point(1, 3);
            lblLinkageType.TabIndex = 12;

            cmbLinkageType.Location = new Point(16, 3);
            cmbLinkageType.Size = new Size(30, 2);
            cmbLinkageType.TabIndex = 13;

            lblParents.Location = new Point(1, 1);
            lblParents.TabIndex = 0;

            txtFather.Location = new Point(14, 1);
            txtFather.ReadOnly = true;
            txtFather.Size = new Size(34, 1);
            txtFather.TabIndex = 1;

            txtMother.Location = new Point(49, 1);
            txtMother.ReadOnly = true;
            txtMother.Size = new Size(34, 1);
            txtMother.TabIndex = 2;

            btnParentsEdit.Text = "~";
            btnParentsEdit.Location = new Point(84, 1);
            btnParentsEdit.Size = new Size(5, 1);
            btnParentsEdit.TabIndex = 4;
            btnParentsEdit.Clicked += btnParentsEdit_Click;

            btnFatherAdd.Text = "+";
            btnFatherAdd.Location = new Point(14, 3);
            btnFatherAdd.Size = new Size(5, 1);
            btnFatherAdd.TabIndex = 6;
            btnFatherAdd.Clicked += btnFatherAdd_Click;

            btnFatherDelete.Text = "x";
            btnFatherDelete.Location = new Point(20, 3);
            btnFatherDelete.Size = new Size(5, 1);
            btnFatherDelete.TabIndex = 7;
            btnFatherDelete.Clicked += btnFatherDelete_Click;

            btnMotherAdd.Text = "+";
            btnMotherAdd.Location = new Point(49, 3);
            btnMotherAdd.Size = new Size(5, 1);
            btnMotherAdd.TabIndex = 9;
            btnMotherAdd.Clicked += btnMotherAdd_Click;

            btnMotherDelete.Text = "x";
            btnMotherDelete.Location = new Point(55, 3);
            btnMotherDelete.Size = new Size(5, 1);
            btnMotherDelete.TabIndex = 10;
            btnMotherDelete.Clicked += btnMotherDelete_Click;

            GroupBox1.Add(lblParents);
            GroupBox1.Add(txtFather);
            GroupBox1.Add(txtMother);
            GroupBox1.Add(btnParentsEdit);
            GroupBox1.Add(btnFatherAdd);
            GroupBox1.Add(btnFatherDelete);
            GroupBox1.Add(btnMotherAdd);
            GroupBox1.Add(btnMotherDelete);
            GroupBox1.Location = new Point(0, 5);
            GroupBox1.Size = new Size(86, 7);
            GroupBox1.TabIndex = 0;
            GroupBox1.TabStop = false;

            Size = new Size(88, 16);
            Add(lblChildName);
            Add(txtChildName);
            Add(lblLinkageType);
            Add(cmbLinkageType);
            Add(GroupBox1);
        }
    }
}
