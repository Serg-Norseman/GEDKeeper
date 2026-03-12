#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class GroupEditDlg
    {
        private FrameView GroupBox1;
        private TextField edName;
        private Label lblName;
        private TabView tabsGroupData;
        private TabView.Tab pageNotes;
        private TabView.Tab pageMultimedia;
        private TabView.Tab pageMembers;
        private Button btnAccept;
        private Button btnCancel;

        private void InitializeComponent()
        {
            GroupBox1 = new FrameView();
            lblName = new Label();
            edName = new TextField();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsGroupData = new TabView();
            pageMembers = new TabView.Tab();
            pageNotes = new TabView.Tab();
            pageMultimedia = new TabView.Tab();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblName.Location = new Point(1, 1);
            lblName.TabIndex = 0;

            edName.Location = new Point(1, 2);
            edName.Size = new Size(62, 1);
            edName.TabIndex = 0;

            GroupBox1.Add(lblName);
            GroupBox1.Add(edName);
            GroupBox1.Location = new Point(1, 0);
            GroupBox1.Size = new Size(66, 6);
            GroupBox1.TabIndex = 0;
            GroupBox1.TabStop = false;

            tabsGroupData.AddTab(pageMembers, true);
            tabsGroupData.AddTab(pageNotes, false);
            tabsGroupData.AddTab(pageMultimedia, false);
            tabsGroupData.Location = new Point(1, 6);
            tabsGroupData.Size = new Size(66, 20);
            tabsGroupData.TabIndex = 1;

            Size = new Size(70, 30);
            Add(GroupBox1);
            Add(tabsGroupData);
        }
    }
}
