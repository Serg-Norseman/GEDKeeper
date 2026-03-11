#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class AssociationEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblRelation;
        private ComboBox cmbRelation;
        private Label lblPerson;
        private TextField txtPerson;
        private Button btnPersonAdd;

        private void InitializeComponent()
        {
            lblRelation = new Label();
            lblPerson = new Label();
            btnPersonAdd = new Button();
            btnAccept = new Button();
            btnCancel = new Button();
            cmbRelation = new ComboBox();
            txtPerson = new TextField();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 5;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 6;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblRelation.Location = new Point(1, 1);
            lblRelation.TabIndex = 0;

            cmbRelation.Location = new Point(1, 2);
            cmbRelation.Size = new Size(46, 2);
            cmbRelation.TabIndex = 1;

            lblPerson.Location = new Point(1, 4);
            lblPerson.TabIndex = 2;

            txtPerson.Location = new Point(1, 5);
            txtPerson.ReadOnly = true;
            txtPerson.Size = new Size(40, 2);
            txtPerson.TabIndex = 3;

            btnPersonAdd.Text = "+";
            btnPersonAdd.Location = new Point(42, 5);
            btnPersonAdd.Size = new Size(5, 1);
            btnPersonAdd.TabIndex = 4;
            btnPersonAdd.Clicked += btnPersonAdd_Click;

            Size = new Size(50, 11);
            Add(lblRelation);
            Add(lblPerson);
            Add(btnPersonAdd);
            Add(cmbRelation);
            Add(txtPerson);
        }
    }
}
