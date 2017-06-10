using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class TaskEditDlg
    {
        private GroupBox GroupBox1;
        private TabControl tabsData;
        private TabPage pageNotes;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPriority;
        private ComboBox txtPriority;
        private Label lblStartDate;
        private MaskedTextBox txtStartDate;
        private MaskedTextBox txtStopDate;
        private Label lblStopDate;
        private Label lblGoal;
        private ComboBox cmbGoalType;
        private TextBox txtGoal;
        private Button btnGoalSelect;

        private void InitializeComponent()
        {
            GroupBox1 = new GroupBox();
            lblPriority = new Label();
            lblStartDate = new Label();
            lblStopDate = new Label();
            lblGoal = new Label();
            btnGoalSelect = new Button();
            txtPriority = new ComboBox();
            txtStartDate = new MaskedTextBox();
            txtStopDate = new MaskedTextBox();
            cmbGoalType = new ComboBox();
            txtGoal = new TextBox();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageNotes = new TabPage();
            GroupBox1.SuspendLayout();
            tabsData.SuspendLayout();
            SuspendLayout();

            GroupBox1.Controls.Add(lblPriority);
            GroupBox1.Controls.Add(lblStartDate);
            GroupBox1.Controls.Add(lblStopDate);
            GroupBox1.Controls.Add(lblGoal);
            GroupBox1.Controls.Add(btnGoalSelect);
            GroupBox1.Controls.Add(txtPriority);
            GroupBox1.Controls.Add(txtStartDate);
            GroupBox1.Controls.Add(txtStopDate);
            GroupBox1.Controls.Add(cmbGoalType);
            GroupBox1.Controls.Add(txtGoal);
            GroupBox1.Dock = DockStyle.Top;
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(674, 118);

            lblPriority.Location = new Point(11, 52);
            lblPriority.Size = new Size(63, 17);
            lblPriority.Text = "lblPriority";

            lblStartDate.Location = new Point(11, 81);
            lblStartDate.Size = new Size(79, 17);
            lblStartDate.Text = "lblStartDate";

            lblStopDate.Location = new Point(343, 81);
            lblStopDate.Size = new Size(78, 17);
            lblStopDate.Text = "lblStopDate";

            lblGoal.Location = new Point(11, 22);
            lblGoal.Size = new Size(46, 17);
            lblGoal.Text = "lblGoal";

            btnGoalSelect.Location = new Point(627, 16);
            btnGoalSelect.Size = new Size(39, 34);
            btnGoalSelect.Click += btnGoalSelect_Click;

            txtPriority.ReadOnly = true;
            txtPriority.Location = new Point(101, 49);
            txtPriority.Size = new Size(225, 25);

            txtStartDate.Location = new Point(101, 78);
            txtStartDate.Mask = "00/00/0000";
            txtStartDate.Size = new Size(225, 24);
            txtStartDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            txtStopDate.Location = new Point(437, 78);
            txtStopDate.Mask = "00/00/0000";
            txtStopDate.Size = new Size(225, 24);
            txtStopDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            cmbGoalType.ReadOnly = true;
            cmbGoalType.Location = new Point(101, 19);
            cmbGoalType.Size = new Size(158, 25);
            cmbGoalType.SelectedIndexChanged += cmbGoalType_SelectedIndexChanged;

            txtGoal.Location = new Point(269, 19);
            txtGoal.ReadOnly = true;
            txtGoal.Size = new Size(348, 24);

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(426, 447);
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(549, 447);
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Controls.Add(pageNotes);
            tabsData.Dock = DockStyle.Top;
            tabsData.Location = new Point(0, 118);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(674, 311);

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(666, 281);
            pageNotes.Text = "pageNotes";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(674, 494);
            Controls.Add(tabsData);
            Controls.Add(GroupBox1);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "TaskEditDlg";
            GroupBox1.ResumeLayout();
            tabsData.ResumeLayout();
            ResumeLayout();
        }
    }
}
