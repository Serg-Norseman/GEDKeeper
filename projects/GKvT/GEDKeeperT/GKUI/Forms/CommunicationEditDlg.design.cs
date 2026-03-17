#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class CommunicationEditDlg
    {
        private FrameView GroupBox1;
        private TabView tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblTheme;
        private TextField txtName;
        private Label lblDate;
        private GKDateBox txtDate;
        private Label lblType;
        private ComboBox cmbCorrType;
        private ComboBox txtDir;
        private Label lblCorresponder;
        private TextField txtCorresponder;
        private Button btnPersonAdd;

        private void InitializeComponent()
        {
            GroupBox1 = new FrameView();
            lblTheme = new Label();
            lblDate = new Label();
            lblType = new Label();
            lblCorresponder = new Label();
            btnPersonAdd = new Button();
            txtName = new TextField();
            txtDate = new GKDateBox();
            cmbCorrType = new ComboBox();
            txtDir = new ComboBox();
            txtCorresponder = new TextField();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            tabsData.AddTab(pageNotes, true);
            tabsData.AddTab(pageMultimedia, false);
            tabsData.Location = new Point(0, 9);
            tabsData.Size = new Size(66, 24);
            tabsData.TabIndex = 1;

            lblTheme.Location = new Point(1, 1);
            lblTheme.TabIndex = 0;

            txtName.Location = new Point(16, 1);
            txtName.Size = new Size(47, 1);
            txtName.TabIndex = 1;

            lblCorresponder.Location = new Point(1, 3);
            lblCorresponder.TabIndex = 2;

            txtDir.Location = new Point(16, 3);
            txtDir.Size = new Size(8, 2);
            txtDir.TabIndex = 3;

            txtCorresponder.Location = new Point(25, 3);
            txtCorresponder.ReadOnly = true;
            txtCorresponder.Size = new Size(32, 1);
            txtCorresponder.TabIndex = 4;

            btnPersonAdd.Text = "+";
            btnPersonAdd.Location = new Point(58, 3);
            btnPersonAdd.Size = new Size(5, 1);
            btnPersonAdd.TabIndex = 5;
            btnPersonAdd.Clicked += btnPersonAdd_Click;

            lblType.Location = new Point(1, 5);
            lblType.TabIndex = 6;

            cmbCorrType.Location = new Point(8, 5);
            cmbCorrType.Size = new Size(16, 2);
            cmbCorrType.TabIndex = 7;

            lblDate.Location = new Point(30, 5);
            lblDate.TabIndex = 8;

            txtDate.Location = new Point(42, 5);
            txtDate.Size = new Size(17, 1);
            txtDate.TabIndex = 9;

            GroupBox1.Add(lblTheme);
            GroupBox1.Add(lblDate);
            GroupBox1.Add(lblType);
            GroupBox1.Add(lblCorresponder);
            GroupBox1.Add(btnPersonAdd);
            GroupBox1.Add(txtName);
            GroupBox1.Add(txtDate);
            GroupBox1.Add(cmbCorrType);
            GroupBox1.Add(txtDir);
            GroupBox1.Add(txtCorresponder);
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(66, 9);
            GroupBox1.TabIndex = 0;
            GroupBox1.TabStop = false;

            Size = new Size(68, 37);
            Add(tabsData);
            Add(GroupBox1);
        }
    }
}
