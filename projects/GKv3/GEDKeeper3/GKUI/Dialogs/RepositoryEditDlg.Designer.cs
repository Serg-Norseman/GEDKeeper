using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class RepositoryEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private GroupBox GroupBox1;
        private Label lblName;
        private TextBox txtName;
        private TabControl tabsData;
        private TabPage pageNotes;
        private Button btnAddress;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            GroupBox1 = new GroupBox();
            lblName = new Label();
            txtName = new TextBox();
            btnAddress = new Button();
            tabsData = new TabControl();
            pageNotes = new TabPage();
            GroupBox1.SuspendLayout();
            tabsData.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(336, 408);
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(459, 408);
            btnCancel.Size = new Size(114, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            GroupBox1.Controls.Add(lblName);
            GroupBox1.Controls.Add(txtName);
            GroupBox1.Dock = DockStyle.Top;
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(586, 50);

            lblName.Location = new Point(12, 18);
            lblName.Size = new Size(67, 17);
            lblName.Text = "lblName";

            txtName.Location = new Point(101, 15);
            txtName.Size = new Size(472, 24);

            btnAddress.Location = new Point(11, 408);
            btnAddress.Size = new Size(114, 30);
            btnAddress.Text = "btnAddress";
            btnAddress.Click += btnAddress_Click;

            tabsData.Controls.Add(pageNotes);
            tabsData.Dock = DockStyle.Top;
            tabsData.Location = new Point(0, 50);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(586, 340);

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(578, 310);
            pageNotes.Text = "pageNotes";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(586, 455);
            Controls.Add(tabsData);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(GroupBox1);
            Controls.Add(btnAddress);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "RepositoryEditDlg";
            GroupBox1.ResumeLayout();
            tabsData.ResumeLayout();
            ResumeLayout();
        }
    }
}
