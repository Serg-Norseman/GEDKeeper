using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class FamilyEditDlg
    {
        private TabControl tabsFamilyData;
        private TabPage pageEvents;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageSources;
        private TabPage pageChilds;
        private Button btnAccept;
        private Button btnCancel;
        private GroupBox GroupBox1;
        private Label lblHusband;
        private TextBox txtHusband;
        private Button btnHusbandAdd;
        private Button btnHusbandDelete;
        private Button btnHusbandSel;
        private Button btnWifeSel;
        private Button btnWifeDelete;
        private Button btnWifeAdd;
        private TextBox txtWife;
        private Label lblWife;
        private Label lblStatus;
        private ComboBox cmbMarriageStatus;
        private Label lblRestriction;
        private ComboBox cmbRestriction;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            GroupBox1 = new GroupBox();
            lblHusband = new Label();
            btnHusbandAdd = new Button();
            btnHusbandDelete = new Button();
            btnHusbandSel = new Button();
            btnWifeSel = new Button();
            btnWifeDelete = new Button();
            btnWifeAdd = new Button();
            lblWife = new Label();
            lblStatus = new Label();
            txtHusband = new TextBox();
            txtWife = new TextBox();
            cmbMarriageStatus = new ComboBox();
            lblRestriction = new Label();
            cmbRestriction = new ComboBox();
            tabsFamilyData = new TabControl();
            pageChilds = new TabPage();
            pageEvents = new TabPage();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            pageSources = new TabPage();
            GroupBox1.SuspendLayout();
            tabsFamilyData.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(462, 491);
            btnAccept.Size = new Size(114, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(582, 491);
            btnCancel.Size = new Size(114, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            GroupBox1.Controls.Add(lblHusband);
            GroupBox1.Controls.Add(btnHusbandAdd);
            GroupBox1.Controls.Add(btnHusbandDelete);
            GroupBox1.Controls.Add(btnHusbandSel);
            GroupBox1.Controls.Add(btnWifeSel);
            GroupBox1.Controls.Add(btnWifeDelete);
            GroupBox1.Controls.Add(btnWifeAdd);
            GroupBox1.Controls.Add(lblWife);
            GroupBox1.Controls.Add(lblStatus);
            GroupBox1.Controls.Add(txtHusband);
            GroupBox1.Controls.Add(txtWife);
            GroupBox1.Controls.Add(cmbMarriageStatus);
            GroupBox1.Dock = DockStyle.Top;
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Padding = new Padding(2);
            GroupBox1.Size = new Size(708, 158);
            GroupBox1.Text = "GroupBox1";

            lblHusband.Location = new Point(5, 35);
            lblHusband.Size = new Size(74, 17);
            lblHusband.Text = "lblHusband";

            btnHusbandAdd.Enabled = false;
            btnHusbandAdd.Location = new Point(576, 26);
            btnHusbandAdd.Size = new Size(39, 34);
            btnHusbandAdd.Click += btnHusbandAddClick;

            btnHusbandDelete.Enabled = false;
            btnHusbandDelete.Location = new Point(620, 26);
            btnHusbandDelete.Size = new Size(39, 34);
            btnHusbandDelete.Click += btnHusbandDeleteClick;

            btnHusbandSel.Location = new Point(664, 26);
            btnHusbandSel.Size = new Size(39, 34);
            btnHusbandSel.Click += btnHusbandSelClick;

            btnWifeSel.Location = new Point(664, 62);
            btnWifeSel.Size = new Size(39, 34);
            btnWifeSel.Click += btnWifeSelClick;

            btnWifeDelete.Enabled = false;
            btnWifeDelete.Location = new Point(620, 62);
            btnWifeDelete.Size = new Size(39, 34);
            btnWifeDelete.Click += btnWifeDeleteClick;

            btnWifeAdd.Enabled = false;
            btnWifeAdd.Location = new Point(576, 62);
            btnWifeAdd.Size = new Size(39, 34);
            btnWifeAdd.Click += btnWifeAddClick;

            lblWife.Location = new Point(5, 71);
            lblWife.Size = new Size(47, 17);
            lblWife.Text = "lblWife";

            lblStatus.Location = new Point(5, 111);
            lblStatus.Size = new Size(59, 17);
            lblStatus.Text = "lblStatus";

            txtHusband.TextColor = SystemColors.Control;
            txtHusband.Location = new Point(110, 31);
            txtHusband.ReadOnly = true;
            txtHusband.Size = new Size(460, 24);

            txtWife.TextColor = SystemColors.Control;
            txtWife.Location = new Point(110, 68);
            txtWife.ReadOnly = true;
            txtWife.Size = new Size(460, 24);

            cmbMarriageStatus.ReadOnly = true;
            cmbMarriageStatus.Location = new Point(110, 108);
            cmbMarriageStatus.Size = new Size(203, 25);

            lblRestriction.Location = new Point(12, 498);
            lblRestriction.Size = new Size(84, 17);
            lblRestriction.Text = "lblRestriction";

            cmbRestriction.ReadOnly = true;
            cmbRestriction.Location = new Point(224, 495);
            cmbRestriction.Size = new Size(203, 25);
            cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

            tabsFamilyData.Controls.Add(pageChilds);
            tabsFamilyData.Controls.Add(pageEvents);
            tabsFamilyData.Controls.Add(pageNotes);
            tabsFamilyData.Controls.Add(pageMultimedia);
            tabsFamilyData.Controls.Add(pageSources);
            tabsFamilyData.Dock = DockStyle.Top;
            tabsFamilyData.Location = new Point(0, 158);
            tabsFamilyData.SelectedIndex = 0;
            tabsFamilyData.Size = new Size(708, 320);

            pageChilds.Location = new Point(4, 26);
            pageChilds.Size = new Size(700, 290);
            pageChilds.Text = "pageChilds";

            pageEvents.Location = new Point(4, 26);
            pageEvents.Size = new Size(700, 290);
            pageEvents.Text = "pageEvents";

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(700, 290);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Location = new Point(4, 26);
            pageMultimedia.Size = new Size(700, 290);
            pageMultimedia.Text = "pageMultimedia";

            pageSources.Location = new Point(4, 26);
            pageSources.Size = new Size(700, 290);
            pageSources.Text = "pageSources";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(708, 535);
            Controls.Add(tabsFamilyData);
            Controls.Add(GroupBox1);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(lblRestriction);
            Controls.Add(cmbRestriction);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "FamilyEditDlg";
            GroupBox1.ResumeLayout();
            tabsFamilyData.ResumeLayout();
            ResumeLayout();
        }
    }
}
