#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class ResearchEditDlg
    {
        private FrameView GroupBox1;
        private TextField txtName;
        private Label lblName;
        private TabView tabsData;
        private TabView.Tab pageNotes;
        private TabView.Tab pageTasks;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPriority;
        private ComboBox cmbPriority;
        private TabView.Tab pageCommunications;
        private Label lblStatus;
        private ComboBox cmbStatus;
        private Label lblStartDate;
        private GKDateBox txtStartDate;
        private Label lblStopDate;
        private GKDateBox txtStopDate;
        private Label lblPercent;
        private NumericStepper nudPercent;
        private TabView.Tab pageGroups;

        private void InitializeComponent()
        {
            GroupBox1 = new FrameView();
            lblName = new Label();
            lblPriority = new Label();
            lblStatus = new Label();
            lblStartDate = new Label();
            lblStopDate = new Label();
            lblPercent = new Label();
            txtName = new TextField();
            cmbPriority = new ComboBox();
            cmbStatus = new ComboBox();
            txtStartDate = new GKDateBox();
            txtStopDate = new GKDateBox();
            nudPercent = new NumericStepper();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageTasks = new TabView.Tab();
            pageCommunications = new TabView.Tab();
            pageGroups = new TabView.Tab();
            pageNotes = new TabView.Tab();

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

            txtName.Location = new Point(16, 1);
            txtName.Size = new Size(65, 1);
            txtName.TabIndex = 1;

            lblPriority.Location = new Point(1, 3);
            lblPriority.TabIndex = 2;

            cmbPriority.Location = new Point(16, 3);
            cmbPriority.Size = new Size(14, 2);
            cmbPriority.TabIndex = 3;

            lblStatus.Location = new Point(33, 3);
            lblStatus.TabIndex = 4;

            cmbStatus.Location = new Point(43, 3);
            cmbStatus.Size = new Size(16, 2);
            cmbStatus.TabIndex = 5;

            lblPercent.Location = new Point(62, 3);
            lblPercent.TabIndex = 6;

            nudPercent.Step = 5;
            nudPercent.Location = new Point(73, 3);
            nudPercent.Size = new Size(8, 1);
            nudPercent.TabIndex = 7;

            lblStartDate.Location = new Point(1, 5);
            lblStartDate.TabIndex = 8;

            txtStartDate.Location = new Point(16, 5);
            txtStartDate.Size = new Size(12, 1);
            txtStartDate.TabIndex = 9;

            lblStopDate.Location = new Point(36, 5);
            lblStopDate.TabIndex = 10;

            txtStopDate.Location = new Point(50, 5);
            txtStopDate.Size = new Size(12, 1);
            txtStopDate.TabIndex = 11;

            GroupBox1.Add(lblName);
            GroupBox1.Add(lblPriority);
            GroupBox1.Add(lblStatus);
            GroupBox1.Add(lblStartDate);
            GroupBox1.Add(lblStopDate);
            GroupBox1.Add(lblPercent);
            GroupBox1.Add(txtName);
            GroupBox1.Add(cmbPriority);
            GroupBox1.Add(cmbStatus);
            GroupBox1.Add(txtStartDate);
            GroupBox1.Add(txtStopDate);
            GroupBox1.Add(nudPercent);
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(84, 9);
            GroupBox1.TabIndex = 0;
            GroupBox1.TabStop = false;

            tabsData.AddTab(pageTasks, true);
            tabsData.AddTab(pageCommunications, false);
            tabsData.AddTab(pageGroups, false);
            tabsData.AddTab(pageNotes, false);
            tabsData.Location = new Point(0, 9);
            tabsData.Size = new Size(84, 25);
            tabsData.TabIndex = 1;

            Size = new Size(86, 38);
            Add(tabsData);
            Add(GroupBox1);
        }
    }
}
