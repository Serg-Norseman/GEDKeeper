#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class LocationLinkEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblLocation;
        private TextField txtTopLevel;
        private Label lblDate;
        private GKDateControl dateCtl;
        private Button btnLocationAdd;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            lblLocation = new Label();
            txtTopLevel = new TextField();
            lblDate = new Label();
            dateCtl = new GKDateControl();
            btnLocationAdd = new Button();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 5;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 6;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblLocation.Location = new Point(1, 1);
            lblLocation.TabIndex = 0;

            txtTopLevel.Location = new Point(16, 1);
            txtTopLevel.ReadOnly = true;
            txtTopLevel.Size = new Size(47, 1);
            txtTopLevel.TabIndex = 1;

            btnLocationAdd.Text = "+";
            btnLocationAdd.Location = new Point(64, 1);
            btnLocationAdd.Size = new Size(5, 1);
            btnLocationAdd.TabIndex = 2;
            btnLocationAdd.Clicked += btnLocationAdd_Click;

            lblDate.Location = new Point(1, 3);
            lblDate.TabIndex = 3;

            dateCtl.Location = new Point(1, 4);
            dateCtl.Size = new Size(68, 6);
            dateCtl.TabIndex = 4;

            Size = new Size(72, 15);
            Add(lblLocation);
            Add(txtTopLevel);
            Add(lblDate);
            Add(dateCtl);
            Add(btnLocationAdd);
        }
    }
}
