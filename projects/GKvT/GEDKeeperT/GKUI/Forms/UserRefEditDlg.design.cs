#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
	partial class UserRefEditDlg
	{
		private Button btnAccept;
		private Button btnCancel;
		private Label lblReference;
		private ComboBox cmbRef;
		private Label lblRefType;
		private ComboBox cmbRefType;

		private void InitializeComponent()
		{
			btnAccept = new Button();
			btnCancel = new Button();
			lblReference = new Label();
			cmbRef = new ComboBox();
			lblRefType = new Label();
			cmbRefType = new ComboBox();

			lblReference.Location = new Point(1, 1);
			lblReference.TabIndex = 0;

			cmbRef.Location = new Point(1, 2);
			cmbRef.Size = new Size(46, 1);
			cmbRef.TabIndex = 1;

			lblRefType.Location = new Point(1, 4);
			lblRefType.TabIndex = 2;

			cmbRefType.Location = new Point(1, 5);
			cmbRefType.Size = new Size(46, 1);
			cmbRefType.TabIndex = 3;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 4;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 5;
            btnCancel.Clicked += CancelClickHandler;

            Size = new Size(50, 11);
			Add(lblReference);
			Add(cmbRef);
			Add(lblRefType);
			Add(cmbRefType);
            AddButton(btnAccept);
            AddButton(btnCancel);
        }
    }
}
