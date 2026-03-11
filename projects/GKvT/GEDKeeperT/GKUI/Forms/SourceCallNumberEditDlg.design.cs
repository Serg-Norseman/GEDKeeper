#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class SourceCallNumberEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblCallNumber;
        private TextField txtNumber;
        private Label lblMediaType;
        private ComboBox cmbMediaType;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            lblCallNumber = new Label();
            txtNumber = new TextField();
            lblMediaType = new Label();
            cmbMediaType = new ComboBox();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 4;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 5;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblCallNumber.Location = new Point(1, 1);
            lblCallNumber.TabIndex = 0;

            txtNumber.Location = new Point(16, 1);
            txtNumber.Size = new Size(30, 1);
            txtNumber.TabIndex = 1;

            lblMediaType.Location = new Point(1, 3);
            lblMediaType.TabIndex = 2;

            cmbMediaType.Location = new Point(16, 3);
            cmbMediaType.Size = new Size(30, 2);
            cmbMediaType.TabIndex = 3;

            Size = new Size(49, 8);
            Add(lblCallNumber);
            Add(txtNumber);
            Add(lblMediaType);
            Add(cmbMediaType);
        }
    }
}
