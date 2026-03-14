#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class MediaFileEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Button btnView;
        private Label lblName;
        private TextField txtName;
        private Label lblType;
        private ComboBox cmbMediaType;
        private Label lblStoreType;
        private ComboBox cmbStoreType;
        private Label lblFile;
        private TextField txtFile;
        private Button btnFileSelect;

        private void InitializeComponent()
        {
            lblName = new Label();
            lblType = new Label();
            lblStoreType = new Label();
            lblFile = new Label();
            txtName = new TextField();
            cmbMediaType = new ComboBox();
            cmbStoreType = new ComboBox();
            txtFile = new TextField();
            btnFileSelect = new Button();
            btnAccept = new Button();
            btnCancel = new Button();
            btnView = new Button();

            btnView.Size = new Size(16, 1);
            btnView.TabIndex = 1;
            btnView.Clicked += btnView_Click;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnView);
            AddButton(btnAccept);
            AddButton(btnCancel);

            lblName.Location = new Point(1, 1);
            lblName.TabIndex = 0;

            txtName.Location = new Point(1, 2);
            txtName.Size = new Size(70, 1);
            txtName.TabIndex = 1;
            txtName.TextChanged += edName_TextChanged;

            lblFile.Location = new Point(1, 4);
            lblFile.TabIndex = 2;

            txtFile.Location = new Point(1, 5);
            txtFile.ReadOnly = true;
            txtFile.Size = new Size(62, 1);
            txtFile.TabIndex = 3;

            btnFileSelect.Text = "...";
            btnFileSelect.Location = new Point(64, 5);
            btnFileSelect.Size = new Size(7, 1);
            btnFileSelect.TabIndex = 4;
            btnFileSelect.Clicked += btnFileSelect_Click;

            lblType.Location = new Point(1, 7);
            lblType.TabIndex = 5;

            cmbMediaType.Location = new Point(1, 8);
            cmbMediaType.Size = new Size(20, 2);
            cmbMediaType.TabIndex = 6;

            lblStoreType.Location = new Point(23, 7);
            lblStoreType.TabIndex = 7;

            cmbStoreType.Location = new Point(23, 8);
            cmbStoreType.Size = new Size(30, 2);
            cmbStoreType.TabIndex = 8;
            cmbStoreType.SelectedItemChanged += cmbStoreType_SelectedIndexChanged;

            Size = new Size(74, 14);
            Add(lblName);
            Add(lblType);
            Add(lblStoreType);
            Add(lblFile);
            Add(txtName);
            Add(cmbMediaType);
            Add(cmbStoreType);
            Add(txtFile);
            Add(btnFileSelect);
        }
    }
}
