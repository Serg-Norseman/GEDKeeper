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
            SuspendLayout();

            lblHusband = new Label();
            //lblHusband.Size = new Size(74, 17);
            lblHusband.Text = "lblHusband";

            lblWife = new Label();
            //lblWife.Size = new Size(47, 17);
            lblWife.Text = "lblWife";

            btnHusbandAdd = new Button();
            btnHusbandAdd.Enabled = false;
            btnHusbandAdd.Size = new Size(26, 26);
            btnHusbandAdd.Click += btnHusbandAddClick;
            btnHusbandAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");

            btnHusbandDelete = new Button();
            btnHusbandDelete.Enabled = false;
            btnHusbandDelete.Size = new Size(26, 26);
            btnHusbandDelete.Click += btnHusbandDeleteClick;
            btnHusbandDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");

            btnHusbandSel = new Button();
            btnHusbandSel.Size = new Size(26, 26);
            btnHusbandSel.Click += btnHusbandSelClick;
            btnHusbandSel.Image = Bitmap.FromResource("Resources.btn_jump.gif");

            btnWifeSel = new Button();
            btnWifeSel.Size = new Size(26, 26);
            btnWifeSel.Click += btnWifeSelClick;
            btnWifeSel.Image = Bitmap.FromResource("Resources.btn_jump.gif");

            btnWifeDelete = new Button();
            btnWifeDelete.Enabled = false;
            btnWifeDelete.Size = new Size(26, 26);
            btnWifeDelete.Click += btnWifeDeleteClick;
            btnWifeDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");

            btnWifeAdd = new Button();
            btnWifeAdd.Enabled = false;
            btnWifeAdd.Size = new Size(26, 26);
            btnWifeAdd.Click += btnWifeAddClick;
            btnWifeAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");

            txtHusband = new TextBox();
            txtHusband.Enabled = false;
            //txtHusband.Size = new Size(460, 24);

            txtWife = new TextBox();
            txtWife.Enabled = false;
            //txtWife.Size = new Size(460, 24);

            lblStatus = new Label();
            lblStatus.Text = "lblStatus";

            cmbMarriageStatus = new ComboBox();
            cmbMarriageStatus.ReadOnly = true;
            //cmbMarriageStatus.Size = new Size(203, 25);

            GroupBox1 = new GroupBox();
            GroupBox1.Text = "GroupBox1";
            GroupBox1.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblHusband, TableLayout.Horizontal(10, new TableCell(txtHusband, true), btnHusbandAdd, btnHusbandDelete, btnHusbandSel) }
                    },
                    new TableRow {
                        Cells = { lblWife, TableLayout.Horizontal(10, new TableCell(txtWife, true), btnWifeAdd, btnWifeDelete, btnWifeSel) }
                    },
                    new TableRow {
                        //ScaleHeight = true,
                        Cells = { lblStatus, cmbMarriageStatus }
                    }
                }
            };

            //

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

            tabsFamilyData = new TabControl();
            tabsFamilyData.Pages.Add(pageChilds);
            tabsFamilyData.Pages.Add(pageEvents);
            tabsFamilyData.Pages.Add(pageNotes);
            tabsFamilyData.Pages.Add(pageMultimedia);
            tabsFamilyData.Pages.Add(pageSources);

            lblRestriction = new Label();
            lblRestriction.Text = "lblRestriction";

            cmbRestriction = new ComboBox();
            cmbRestriction.ReadOnly = true;
            //cmbRestriction.Size = new Size(203, 25);
            cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;
            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { GroupBox1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsFamilyData }
                    },
                    UIHelper.MakeDialogFooter(lblRestriction, cmbRestriction, null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "FamilyEditDlg";

            SetPredefProperties(700, 540);
            ResumeLayout();
        }
    }
}
