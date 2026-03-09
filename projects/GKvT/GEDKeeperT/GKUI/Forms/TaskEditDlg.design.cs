#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TaskEditDlg
    {
        private FrameView GroupBox1;
        private TabView tabsData;
        private TabView.Tab pageNotes;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPriority;
        private ComboBox txtPriority;
        private Label lblStartDate;
        private GKDateBox txtStartDate;
        private GKDateBox txtStopDate;
        private Label lblStopDate;
        private Label lblGoal;
        private ComboBox cmbGoalType;
        private TextField txtGoal;
        private Button btnGoalSelect;

        private void InitializeComponent()
        {
            GroupBox1 = new FrameView();
            lblPriority = new Label();
            lblStartDate = new Label();
            lblStopDate = new Label();
            lblGoal = new Label();
            btnGoalSelect = new Button();
            txtPriority = new ComboBox();
            txtStartDate = new GKDateBox();
            txtStopDate = new GKDateBox();
            cmbGoalType = new ComboBox();
            txtGoal = new TextField();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageNotes = new TabView.Tab();

            GroupBox1.Add(lblPriority);
            GroupBox1.Add(lblStartDate);
            GroupBox1.Add(lblStopDate);
            GroupBox1.Add(lblGoal);
            GroupBox1.Add(btnGoalSelect);
            GroupBox1.Add(txtPriority);
            GroupBox1.Add(txtStartDate);
            GroupBox1.Add(txtStopDate);
            GroupBox1.Add(cmbGoalType);
            GroupBox1.Add(txtGoal);
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(64, 7);
            GroupBox1.TabIndex = 0;
            GroupBox1.TabStop = false;

            lblGoal.Location = new Point(0, 0);
            lblGoal.TabIndex = 0;

            lblPriority.Location = new Point(0, 2);
            lblPriority.TabIndex = 4;

            lblStartDate.Location = new Point(0, 4);
            lblStartDate.TabIndex = 6;

            lblStopDate.Location = new Point(32, 4);
            lblStopDate.TabIndex = 8;

            cmbGoalType.Location = new Point(12, 0);
            cmbGoalType.Size = new Size(10, 1);
            cmbGoalType.TabIndex = 1;
            cmbGoalType.SelectedItemChanged += cmbGoalType_SelectedIndexChanged;

            txtGoal.Location = new Point(23, 0);
            txtGoal.ReadOnly = true;
            txtGoal.Size = new Size(32, 1);
            txtGoal.TabIndex = 2;

            btnGoalSelect.Text = "+";
            btnGoalSelect.Location = new Point(56, 0);
            btnGoalSelect.Size = new Size(5, 1);
            btnGoalSelect.TabIndex = 3;
            btnGoalSelect.Clicked += btnGoalSelect_Click;

            txtPriority.Location = new Point(12, 2);
            txtPriority.Size = new Size(15, 1);
            txtPriority.TabIndex = 5;

            txtStartDate.Location = new Point(12, 4);
            txtStartDate.Size = new Size(12, 1);
            txtStartDate.TabIndex = 7;

            txtStopDate.Location = new Point(44, 4);
            txtStopDate.Size = new Size(12, 1);
            txtStopDate.TabIndex = 9;

            tabsData.AddTab(pageNotes, true);
            tabsData.Location = new Point(0, 7);
            tabsData.Size = new Size(66, 20);
            tabsData.TabIndex = 1;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            Size = new Size(68, 32);
            Add(tabsData);
            Add(GroupBox1);
            AddButton(btnAccept);
            AddButton(btnCancel);
        }
    }
}
