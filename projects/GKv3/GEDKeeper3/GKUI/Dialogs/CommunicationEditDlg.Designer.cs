using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class CommunicationEditDlg
    {
        private GroupBox GroupBox1;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblTheme;
        private TextBox txtName;
        private Label lblDate;
        private MaskedTextBox txtDate;
        private Label lblType;
        private ComboBox cmbCorrType;
        private ComboBox txtDir;
        private Label lblCorresponder;
        private TextBox txtCorresponder;
        private Button btnPersonAdd;

        private void InitializeComponent()
        {
            GroupBox1 = new GroupBox();
            lblTheme = new Label();
            lblDate = new Label();
            lblType = new Label();
            lblCorresponder = new Label();
            btnPersonAdd = new Button();
            txtName = new TextBox();
            txtDate = new MaskedTextBox();
            cmbCorrType = new ComboBox();
            txtDir = new ComboBox();
            txtCorresponder = new TextBox();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            GroupBox1.SuspendLayout();
            tabsData.SuspendLayout();
            SuspendLayout();

            GroupBox1.Controls.Add(lblTheme);
            GroupBox1.Controls.Add(lblDate);
            GroupBox1.Controls.Add(lblType);
            GroupBox1.Controls.Add(lblCorresponder);
            GroupBox1.Controls.Add(btnPersonAdd);
            GroupBox1.Controls.Add(txtName);
            GroupBox1.Controls.Add(txtDate);
            GroupBox1.Controls.Add(cmbCorrType);
            GroupBox1.Controls.Add(txtDir);
            GroupBox1.Controls.Add(txtCorresponder);
            GroupBox1.Dock = DockStyle.Top;
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(675, 118);

            lblTheme.Location = new Point(11, 22);
            lblTheme.Size = new Size(62, 17);
            lblTheme.Text = "lblTheme";

            lblDate.Location = new Point(322, 81);
            lblDate.Size = new Size(49, 17);
            lblDate.Text = "lblDate";

            lblType.Location = new Point(11, 81);
            lblType.Size = new Size(51, 17);
            lblType.Text = "lblType";

            lblCorresponder.Location = new Point(11, 53);
            lblCorresponder.Size = new Size(104, 17);
            lblCorresponder.Text = "lblCorresponder";

            btnPersonAdd.Location = new Point(627, 45);
            btnPersonAdd.Size = new Size(37, 32);
            btnPersonAdd.Click += btnPersonAdd_Click;

            txtName.Location = new Point(134, 19);
            txtName.Size = new Size(528, 24);

            txtDate.Location = new Point(392, 78);
            txtDate.Mask = "00/00/0000";
            txtDate.Size = new Size(225, 24);
            txtDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            cmbCorrType.ReadOnly = true;
            cmbCorrType.Location = new Point(134, 78);
            cmbCorrType.Size = new Size(147, 25);

            txtDir.ReadOnly = true;
            txtDir.Location = new Point(134, 49);
            txtDir.Size = new Size(91, 25);

            txtCorresponder.TextColor = SystemColors.Control;
            txtCorresponder.Location = new Point(235, 49);
            txtCorresponder.ReadOnly = true;
            txtCorresponder.Size = new Size(382, 24);

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(426, 466);
            btnAccept.Size = new Size(113, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(549, 466);
            btnCancel.Size = new Size(113, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Controls.Add(pageNotes);
            tabsData.Controls.Add(pageMultimedia);
            tabsData.Dock = DockStyle.Top;
            tabsData.Location = new Point(0, 118);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(675, 330);

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(667, 300);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Location = new Point(4, 26);
            pageMultimedia.Size = new Size(667, 300);
            pageMultimedia.Text = "pageMultimedia";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(675, 513);
            Controls.Add(tabsData);
            Controls.Add(GroupBox1);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "CommunicationEditDlg";
            GroupBox1.ResumeLayout();
            tabsData.ResumeLayout();
            ResumeLayout();
        }
    }
}
