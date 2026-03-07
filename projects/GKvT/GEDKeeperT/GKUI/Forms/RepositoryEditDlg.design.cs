#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
	partial class RepositoryEditDlg
	{
		private Button btnAccept;
		private Button btnCancel;
		private FrameView GroupBox1;
		private Label lblName;
		private TextField txtName;
		private TabView tabsData;
		private TabView.Tab pageNotes;
		private Button btnAddress;
        private TabView.Tab pageUserRefs;

        private void InitializeComponent()
		{
			btnAccept = new Button();
			btnCancel = new Button();
			GroupBox1 = new FrameView();
			lblName = new Label();
			txtName = new TextField();
			btnAddress = new Button();
			tabsData = new TabView();
			pageNotes = new TabView.Tab();
            pageUserRefs = new TabView.Tab();

            lblName.Location = new Point(1, 1);
            lblName.TabIndex = 0;

            txtName.Location = new Point(1, 2);
            txtName.Size = new Size(52, 1);
            txtName.TabIndex = 1;

            GroupBox1.Add(lblName);
			GroupBox1.Add(txtName);
			GroupBox1.Location = new Point(1, 0);
			GroupBox1.Size = new Size(56, 6);
			GroupBox1.TabIndex = 0;
			GroupBox1.TabStop = false;

			tabsData.AddTab(pageNotes, false);
            tabsData.AddTab(pageUserRefs, false);
			tabsData.Location = new Point(1, 6);
			tabsData.Size = new Size(56, 20);
			tabsData.TabIndex = 1;

            btnAddress.Size = new Size(16, 1);
            btnAddress.TabIndex = 2;
            btnAddress.Clicked += btnAddress_Click;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 3;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 4;
            btnCancel.Clicked += CancelClickHandler;

            Size = new Size(60, 30);
			Add(tabsData);
			Add(GroupBox1);
            AddButton(btnAddress);
            AddButton(btnAccept);
            AddButton(btnCancel);
        }
    }
}
