using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

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

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            lblHusband.Size = new Size(74, 17);
            lblHusband.Text = "lblHusband";

            lblWife.Size = new Size(47, 17);
            lblWife.Text = "lblWife";

            btnHusbandAdd.Enabled = false;
            btnHusbandAdd.Size = new Size(39, 34);
            btnHusbandAdd.Click += btnHusbandAddClick;

            btnHusbandDelete.Enabled = false;
            btnHusbandDelete.Size = new Size(39, 34);
            btnHusbandDelete.Click += btnHusbandDeleteClick;

            btnHusbandSel.Size = new Size(39, 34);
            btnHusbandSel.Click += btnHusbandSelClick;

            btnWifeSel.Size = new Size(39, 34);
            btnWifeSel.Click += btnWifeSelClick;

            btnWifeDelete.Enabled = false;
            btnWifeDelete.Size = new Size(39, 34);
            btnWifeDelete.Click += btnWifeDeleteClick;

            btnWifeAdd.Enabled = false;
            btnWifeAdd.Size = new Size(39, 34);
            btnWifeAdd.Click += btnWifeAddClick;

            txtHusband.TextColor = SystemColors.Control;
            txtHusband.ReadOnly = true;
            txtHusband.Size = new Size(460, 24);

            txtWife.TextColor = SystemColors.Control;
            txtWife.ReadOnly = true;
            txtWife.Size = new Size(460, 24);

            lblStatus.Text = "lblStatus";

            cmbMarriageStatus.ReadOnly = true;
            cmbMarriageStatus.Size = new Size(203, 25);

            GroupBox1.Text = "GroupBox1";
            GroupBox1.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblHusband, txtHusband, btnHusbandAdd, btnHusbandDelete, btnHusbandSel }
                    },
                    new TableRow {
                        Cells = { lblWife, txtWife, btnWifeAdd, btnWifeDelete, btnWifeSel }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { lblStatus, cmbMarriageStatus }
                    }
                }
            };

            pageChilds = new TabPage();
            pageChilds.Text = "pageChilds";

            pageEvents = new TabPage();
            pageEvents.Text = "pageEvents";

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            pageMultimedia = new TabPage();
            pageMultimedia.Text = "pageMultimedia";

            pageSources = new TabPage();
            pageSources.Text = "pageSources";

            tabsFamilyData.Pages.Add(pageChilds);
            tabsFamilyData.Pages.Add(pageEvents);
            tabsFamilyData.Pages.Add(pageNotes);
            tabsFamilyData.Pages.Add(pageMultimedia);
            tabsFamilyData.Pages.Add(pageSources);
            tabsFamilyData.SelectedIndex = 0;

            lblRestriction.Text = "lblRestriction";

            cmbRestriction.ReadOnly = true;
            cmbRestriction.Size = new Size(203, 25);
            cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { GroupBox1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsFamilyData }
                    },
                    new TableRow {
                        Cells = { lblRestriction, cmbRestriction, null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(708, 535);
            Title = "FamilyEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
