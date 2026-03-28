#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class LocationNameEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblTitle;
        private TextField txtTitle;
        private Label lblShortTitle;
        private TextField txtShortTitle;
        private Label lblDate;
        private GKDateControl dateCtl;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            lblTitle = new Label();
            txtTitle = new TextField();
            lblShortTitle = new Label();
            txtShortTitle = new TextField();
            lblDate = new Label();
            dateCtl = new GKDateControl();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 6;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 7;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblTitle.Location = new Point(1, 1);
            lblTitle.TabIndex = 0;

            txtTitle.Location = new Point(16, 1);
            txtTitle.Size = new Size(52, 1);
            txtTitle.TabIndex = 1;

            lblShortTitle.Location = new Point(1, 3);
            lblShortTitle.TabIndex = 2;

            txtShortTitle.Location = new Point(20, 3);
            txtShortTitle.Size = new Size(48, 24);
            txtShortTitle.TabIndex = 3;

            lblDate.Location = new Point(1, 5);
            lblDate.TabIndex = 4;

            dateCtl.Location = new Point(1, 6);
            dateCtl.Size = new Size(68, 6);
            dateCtl.TabIndex = 5;

            Size = new Size(72, 16);
            Add(lblTitle);
            Add(txtTitle);
            Add(lblShortTitle);
            Add(txtShortTitle);
            Add(lblDate);
            Add(dateCtl);
        }
    }
}
