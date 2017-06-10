using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class ResearchEditDlg
    {
        private GroupBox GroupBox1;
        private TextBox txtName;
        private Label lblName;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageTasks;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPriority;
        private ComboBox cmbPriority;
        private TabPage pageCommunications;
        private Label lblStatus;
        private ComboBox cmbStatus;
        private Label lblStartDate;
        private MaskedTextBox txtStartDate;
        private Label lblStopDate;
        private MaskedTextBox txtStopDate;
        private Label lblPercent;
        private NumericUpDown nudPercent;
        private TabPage pageGroups;

        private void InitializeComponent()
        {
            GroupBox1 = new GroupBox();
            lblName = new Label();
            lblPriority = new Label();
            lblStatus = new Label();
            lblStartDate = new Label();
            lblStopDate = new Label();
            lblPercent = new Label();
            txtName = new TextBox();
            cmbPriority = new ComboBox();
            cmbStatus = new ComboBox();
            txtStartDate = new MaskedTextBox();
            txtStopDate = new MaskedTextBox();
            nudPercent = new NumericUpDown();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageTasks = new TabPage();
            pageCommunications = new TabPage();
            pageGroups = new TabPage();
            pageNotes = new TabPage();
            GroupBox1.SuspendLayout();
            tabsData.SuspendLayout();
            SuspendLayout();

            GroupBox1.Controls.Add(lblName);
            GroupBox1.Controls.Add(lblPriority);
            GroupBox1.Controls.Add(lblStatus);
            GroupBox1.Controls.Add(lblStartDate);
            GroupBox1.Controls.Add(lblStopDate);
            GroupBox1.Controls.Add(lblPercent);
            GroupBox1.Controls.Add(txtName);
            GroupBox1.Controls.Add(cmbPriority);
            GroupBox1.Controls.Add(cmbStatus);
            GroupBox1.Controls.Add(txtStartDate);
            GroupBox1.Controls.Add(txtStopDate);
            GroupBox1.Controls.Add(nudPercent);
            GroupBox1.Dock = DockStyle.Top;
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(852, 118);

            lblName.Location = new Point(11, 22);
            lblName.Size = new Size(67, 17);
            lblName.Text = "lblName";

            lblPriority.Location = new Point(11, 51);
            lblPriority.Size = new Size(80, 17);
            lblPriority.Text = "lblPriority";

            lblStatus.Location = new Point(347, 52);
            lblStatus.Size = new Size(78, 17);
            lblStatus.Text = "lblStatus";

            lblStartDate.Location = new Point(11, 81);
            lblStartDate.Size = new Size(72, 17);
            lblStartDate.Text = "lblStartDate";

            lblStopDate.Location = new Point(347, 81);
            lblStopDate.Size = new Size(77, 17);
            lblStopDate.Text = "lblStopDate";

            lblPercent.Location = new Point(683, 52);
            lblPercent.Size = new Size(64, 17);
            lblPercent.Text = "lblPercent";

            txtName.Location = new Point(101, 19);
            txtName.Size = new Size(740, 24);

            cmbPriority.ReadOnly = true;
            cmbPriority.Location = new Point(101, 49);
            cmbPriority.Size = new Size(225, 25);

            cmbStatus.ReadOnly = true;
            cmbStatus.Location = new Point(437, 49);
            cmbStatus.Size = new Size(225, 25);

            txtStartDate.Location = new Point(101, 78);
            txtStartDate.Mask = "00/00/0000";
            txtStartDate.Size = new Size(225, 24);
            txtStartDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            txtStopDate.Location = new Point(437, 78);
            txtStopDate.Mask = "00/00/0000";
            txtStopDate.Size = new Size(225, 24);
            txtStopDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            nudPercent.Increment = 5;
            nudPercent.Location = new Point(762, 49);
            nudPercent.Size = new Size(57, 24);

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(605, 515);
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(728, 515);
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Controls.Add(pageTasks);
            tabsData.Controls.Add(pageCommunications);
            tabsData.Controls.Add(pageGroups);
            tabsData.Controls.Add(pageNotes);
            tabsData.Dock = DockStyle.Top;
            tabsData.Location = new Point(0, 118);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(852, 379);

            pageTasks.Location = new Point(4, 26);
            pageTasks.Size = new Size(844, 349);
            pageTasks.Text = "pageTasks";

            pageCommunications.Location = new Point(4, 26);
            pageCommunications.Size = new Size(844, 349);
            pageCommunications.Text = "pageCommunications";

            pageGroups.Location = new Point(4, 26);
            pageGroups.Size = new Size(844, 349);
            pageGroups.Text = "pageGroups";

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(844, 349);
            pageNotes.Text = "pageNotes";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(852, 557);
            Controls.Add(tabsData);
            Controls.Add(GroupBox1);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "ResearchEditDlg";
            GroupBox1.ResumeLayout();
            tabsData.ResumeLayout();
            ResumeLayout();
        }
    }
}
