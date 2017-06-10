using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class GroupEditDlg
    {
        private GroupBox GroupBox1;
        private TextBox edName;
        private Label lblName;
        private TabControl tabsGroupData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageMembers;
        private Button btnAccept;
        private Button btnCancel;

        private void InitializeComponent()
        {
            GroupBox1 = new GroupBox();
            lblName = new Label();
            edName = new TextBox();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsGroupData = new TabControl();
            pageMembers = new TabPage();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            GroupBox1.SuspendLayout();
            tabsGroupData.SuspendLayout();
            SuspendLayout();

            GroupBox1.Controls.Add(lblName);
            GroupBox1.Controls.Add(edName);
            GroupBox1.Dock = DockStyle.Top;
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(673, 60);

            lblName.Location = new Point(12, 22);
            lblName.Size = new Size(67, 17);
            lblName.Text = "lblName";

            edName.Location = new Point(101, 19);
            edName.Size = new Size(561, 24);

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(426, 515);
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(549, 515);
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsGroupData.Controls.Add(pageMembers);
            tabsGroupData.Controls.Add(pageNotes);
            tabsGroupData.Controls.Add(pageMultimedia);
            tabsGroupData.Dock = DockStyle.Top;
            tabsGroupData.Location = new Point(0, 60);
            tabsGroupData.SelectedIndex = 0;
            tabsGroupData.Size = new Size(673, 437);

            pageMembers.Location = new Point(4, 26);
            pageMembers.Size = new Size(665, 407);
            pageMembers.Text = "pageMembers";

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(665, 407);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Location = new Point(4, 26);
            pageMultimedia.Size = new Size(665, 407);
            pageMultimedia.Text = "pageMultimedia";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(673, 560);
            Controls.Add(tabsGroupData);
            Controls.Add(btnCancel);
            Controls.Add(btnAccept);
            Controls.Add(GroupBox1);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "GroupEditDlg";
            GroupBox1.ResumeLayout();
            tabsGroupData.ResumeLayout();
            ResumeLayout();
        }
    }
}
