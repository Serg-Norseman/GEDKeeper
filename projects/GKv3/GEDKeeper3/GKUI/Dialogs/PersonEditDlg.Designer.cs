using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class PersonEditDlg
    {
        private TabControl tabsPersonData;
        private TabPage pageEvents;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageSources;
        private TabPage pageSpouses;
        private TabPage pageAssociations;
        private TabPage pageGroups;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblRestriction;
        private ComboBox cmbRestriction;
        private GroupBox GroupBox1;
        private Label lblSurname;
        private Label lblName;
        private Label lblPatronymic;
        private Label lblSex;
        private TextBox txtSurname;
        private TextBox txtName;
        private ComboBox cmbPatronymic;
        public ComboBox cmbSex;
        private CheckBox chkPatriarch;
        private TabPage pageUserRefs;
        private Panel panCtlParents;
        private Label lblParents;
        private TextBox txtFather;
        private TextBox txtMother;
        private Button btnParentsAdd;
        private Button btnParentsEdit;
        private Button btnParentsDelete;
        private CheckBox chkBookmark;
        private Label lblSurnamePrefix;
        private TextBox txtSurnamePrefix;
        private Label lblNamePrefix;
        private TextBox txtNamePrefix;
        private Label lblNameSuffix;
        private TextBox txtNameSuffix;
        private Label lblNickname;
        private TextBox txtNickname;
        private GKUI.Components.GKPortrait imgPortrait;
        private Button btnNameCopy;
        private Button btnPortraitAdd;
        private Button btnPortraitDelete;
        private Button btnFatherAdd;
        private Button btnFatherDelete;
        private Button btnFatherSel;
        private Button btnMotherAdd;
        private Button btnMotherDelete;
        private Button btnMotherSel;
        private TabPage pageNames;
        private TextBox txtMarriedSurname;
        private Label lblMarriedSurname;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            lblRestriction = new Label();
            cmbRestriction = new ComboBox();
            GroupBox1 = new GroupBox();
            imgPortrait = new GKUI.Components.GKPortrait();
            lblMarriedSurname = new Label();
            lblSurname = new Label();
            lblName = new Label();
            lblPatronymic = new Label();
            lblSex = new Label();
            lblSurnamePrefix = new Label();
            lblNamePrefix = new Label();
            lblNameSuffix = new Label();
            lblNickname = new Label();
            btnPortraitAdd = new Button();
            btnPortraitDelete = new Button();
            txtMarriedSurname = new TextBox();
            txtSurname = new TextBox();
            txtName = new TextBox();
            cmbPatronymic = new ComboBox();
            cmbSex = new ComboBox();
            chkPatriarch = new CheckBox();
            panCtlParents = new Panel();
            txtMother = new TextBox();
            lblParents = new Label();
            btnParentsAdd = new Button();
            btnParentsEdit = new Button();
            btnParentsDelete = new Button();
            btnFatherAdd = new Button();
            btnFatherDelete = new Button();
            btnFatherSel = new Button();
            btnMotherAdd = new Button();
            btnMotherDelete = new Button();
            btnMotherSel = new Button();
            txtFather = new TextBox();
            chkBookmark = new CheckBox();
            txtSurnamePrefix = new TextBox();
            txtNamePrefix = new TextBox();
            txtNameSuffix = new TextBox();
            txtNickname = new TextBox();
            btnNameCopy = new Button();
            tabsPersonData = new TabControl();
            pageEvents = new TabPage();
            pageSpouses = new TabPage();
            pageNames = new TabPage();
            pageAssociations = new TabPage();
            pageGroups = new TabPage();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            pageSources = new TabPage();
            pageUserRefs = new TabPage();
            GroupBox1.SuspendLayout();
            panCtlParents.SuspendLayout();
            tabsPersonData.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(502, 529);
            btnAccept.Size = new Size(91, 24);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(600, 529);
            btnCancel.Size = new Size(91, 24);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            lblRestriction.Location = new Point(9, 531);
            lblRestriction.Size = new Size(68, 13);
            lblRestriction.Text = "lblRestriction";

            cmbRestriction.ReadOnly = true;
            cmbRestriction.Location = new Point(179, 529);
            cmbRestriction.Size = new Size(163, 21);
            cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

            GroupBox1.Controls.Add(imgPortrait);
            GroupBox1.Controls.Add(lblMarriedSurname);
            GroupBox1.Controls.Add(lblSurname);
            GroupBox1.Controls.Add(lblName);
            GroupBox1.Controls.Add(lblPatronymic);
            GroupBox1.Controls.Add(lblSex);
            GroupBox1.Controls.Add(lblSurnamePrefix);
            GroupBox1.Controls.Add(lblNamePrefix);
            GroupBox1.Controls.Add(lblNameSuffix);
            GroupBox1.Controls.Add(lblNickname);
            GroupBox1.Controls.Add(btnPortraitAdd);
            GroupBox1.Controls.Add(btnPortraitDelete);
            GroupBox1.Controls.Add(txtMarriedSurname);
            GroupBox1.Controls.Add(txtSurname);
            GroupBox1.Controls.Add(txtName);
            GroupBox1.Controls.Add(cmbPatronymic);
            GroupBox1.Controls.Add(cmbSex);
            GroupBox1.Controls.Add(chkPatriarch);
            GroupBox1.Controls.Add(panCtlParents);
            GroupBox1.Controls.Add(chkBookmark);
            GroupBox1.Controls.Add(txtSurnamePrefix);
            GroupBox1.Controls.Add(txtNamePrefix);
            GroupBox1.Controls.Add(txtNameSuffix);
            GroupBox1.Controls.Add(txtNickname);
            GroupBox1.Dock = DockStyle.Top;
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Padding = new Padding(2);
            GroupBox1.Size = new Size(699, 258);

            imgPortrait.Cursor = Cursors.Arrow;
            imgPortrait.Image = null;
            imgPortrait.Location = new Point(537, 12);
            imgPortrait.Size = new Size(149, 165);
            imgPortrait.SizeMode = PictureBoxSizeMode.Normal;
            imgPortrait.SlidePanelHeight = 36;
            imgPortrait.PixelSpeed = 5;

            lblMarriedSurname.Location = new Point(9, 56);
            lblMarriedSurname.Size = new Size(95, 13);
            lblMarriedSurname.Text = "lblMarriedSurname";

            lblSurname.Location = new Point(9, 15);
            lblSurname.Size = new Size(59, 13);
            lblSurname.Text = "lblSurname";

            lblName.Location = new Point(9, 94);
            lblName.Size = new Size(44, 13);
            lblName.Text = "lblName";

            lblPatronymic.Location = new Point(9, 132);
            lblPatronymic.Size = new Size(70, 13);
            lblPatronymic.Text = "lblPatronymic";

            lblSex.Location = new Point(360, 94);
            lblSex.Size = new Size(35, 13);
            lblSex.Text = "lblSex";

            lblSurnamePrefix.Location = new Point(207, 15);
            lblSurnamePrefix.Size = new Size(87, 13);
            lblSurnamePrefix.Text = "lblSurnamePrefix";

            lblNamePrefix.Location = new Point(207, 54);
            lblNamePrefix.Size = new Size(72, 13);
            lblNamePrefix.Text = "lblNamePrefix";

            lblNameSuffix.Location = new Point(207, 94);
            lblNameSuffix.Size = new Size(72, 13);
            lblNameSuffix.Text = "lblNameSuffix";

            lblNickname.Location = new Point(207, 132);
            lblNickname.Size = new Size(62, 13);
            lblNickname.Text = "lblNickname";

            btnPortraitAdd.Location = new Point(618, 148);
            btnPortraitAdd.Size = new Size(29, 29);
            btnPortraitAdd.Click += btnPortraitAdd_Click;

            btnPortraitDelete.Location = new Point(654, 148);
            btnPortraitDelete.Size = new Size(29, 29);
            btnPortraitDelete.Click += btnPortraitDelete_Click;

            txtMarriedSurname.Location = new Point(9, 70);
            txtMarriedSurname.Size = new Size(182, 21);
            txtMarriedSurname.KeyDown += edSurname_KeyDown;
            //txtMarriedSurname.KeyPress += edSurname_KeyPress;

            txtSurname.Location = new Point(9, 31);
            txtSurname.Size = new Size(182, 21);
            txtSurname.KeyDown += edSurname_KeyDown;
            //txtSurname.KeyPress += edSurname_KeyPress;

            txtName.Location = new Point(9, 108);
            txtName.Size = new Size(182, 21);
            txtName.KeyDown += edSurname_KeyDown;
            //txtName.KeyPress += edSurname_KeyPress;

            cmbPatronymic.Location = new Point(9, 147);
            cmbPatronymic.Size = new Size(182, 21);
            cmbPatronymic.KeyDown += edSurname_KeyDown;
            //cmbPatronymic.KeyPress += edSurname_KeyPress;

            cmbSex.ReadOnly = true;
            cmbSex.Location = new Point(360, 108);
            cmbSex.Size = new Size(154, 21);
            cmbSex.SelectedIndexChanged += cbSex_SelectedIndexChanged;

            chkPatriarch.Location = new Point(358, 140);
            chkPatriarch.Size = new Size(85, 17);
            chkPatriarch.Text = "chkPatriarch";

            panCtlParents.BorderStyle = BorderStyle.FixedSingle;
            panCtlParents.Controls.Add(txtMother);
            panCtlParents.Controls.Add(lblParents);
            panCtlParents.Controls.Add(btnParentsAdd);
            panCtlParents.Controls.Add(btnParentsEdit);
            panCtlParents.Controls.Add(btnParentsDelete);
            panCtlParents.Controls.Add(btnFatherAdd);
            panCtlParents.Controls.Add(btnFatherDelete);
            panCtlParents.Controls.Add(btnFatherSel);
            panCtlParents.Controls.Add(btnMotherAdd);
            panCtlParents.Controls.Add(btnMotherDelete);
            panCtlParents.Controls.Add(btnMotherSel);
            panCtlParents.Controls.Add(txtFather);
            panCtlParents.Location = new Point(2, 186);
            panCtlParents.Size = new Size(696, 69);

            txtMother.TextColor = SystemColors.Control;
            txtMother.Location = new Point(331, 8);
            txtMother.ReadOnly = true;
            txtMother.Size = new Size(252, 21);

            lblParents.Location = new Point(10, 11);
            lblParents.Size = new Size(54, 13);
            lblParents.Text = "lblParents";

            btnParentsAdd.Location = new Point(588, 5);
            btnParentsAdd.Size = new Size(29, 29);
            btnParentsAdd.Click += btnParentsAdd_Click;

            btnParentsEdit.Location = new Point(621, 5);
            btnParentsEdit.Size = new Size(29, 29);
            btnParentsEdit.Click += btnParentsEdit_Click;

            btnParentsDelete.Location = new Point(653, 5);
            btnParentsDelete.Size = new Size(30, 29);
            btnParentsDelete.Click += btnParentsDelete_Click;

            btnFatherAdd.Location = new Point(222, 34);
            btnFatherAdd.Size = new Size(29, 29);
            btnFatherAdd.Click += btnFatherAdd_Click;

            btnFatherDelete.Location = new Point(258, 34);
            btnFatherDelete.Size = new Size(29, 29);
            btnFatherDelete.Click += btnFatherDelete_Click;

            btnFatherSel.Location = new Point(294, 34);
            btnFatherSel.Size = new Size(29, 29);
            btnFatherSel.Click += btnFatherSel_Click;

            btnMotherAdd.Location = new Point(482, 34);
            btnMotherAdd.Size = new Size(29, 29);
            btnMotherAdd.Click += btnMotherAdd_Click;

            btnMotherDelete.Location = new Point(518, 34);
            btnMotherDelete.Size = new Size(29, 29);
            btnMotherDelete.Click += btnMotherDelete_Click;

            btnMotherSel.Location = new Point(554, 34);
            btnMotherSel.Size = new Size(29, 29);
            btnMotherSel.Click += btnMotherSel_Click;

            txtFather.TextColor = SystemColors.Control;
            txtFather.Location = new Point(72, 8);
            txtFather.ReadOnly = true;
            txtFather.Size = new Size(251, 21);

            chkBookmark.Location = new Point(358, 157);
            chkBookmark.Size = new Size(88, 17);
            chkBookmark.Text = "chkBookmark";

            txtSurnamePrefix.Location = new Point(207, 31);
            txtSurnamePrefix.Size = new Size(136, 21);

            txtNamePrefix.Location = new Point(207, 70);
            txtNamePrefix.Size = new Size(136, 21);

            txtNameSuffix.Location = new Point(207, 108);
            txtNameSuffix.Size = new Size(136, 21);

            txtNickname.Location = new Point(207, 147);
            txtNickname.Size = new Size(136, 21);

            btnNameCopy.Location = new Point(430, 529);
            btnNameCopy.Size = new Size(37, 24);
            btnNameCopy.Click += btnNameCopy_Click;

            tabsPersonData.Controls.Add(pageEvents);
            tabsPersonData.Controls.Add(pageSpouses);
            tabsPersonData.Controls.Add(pageNames);
            tabsPersonData.Controls.Add(pageAssociations);
            tabsPersonData.Controls.Add(pageGroups);
            tabsPersonData.Controls.Add(pageNotes);
            tabsPersonData.Controls.Add(pageMultimedia);
            tabsPersonData.Controls.Add(pageSources);
            tabsPersonData.Controls.Add(pageUserRefs);
            tabsPersonData.Dock = DockStyle.Top;
            tabsPersonData.Location = new Point(0, 258);
            tabsPersonData.SelectedIndex = 0;
            tabsPersonData.Size = new Size(699, 256);

            pageEvents.Location = new Point(4, 22);
            pageEvents.Size = new Size(691, 230);
            pageEvents.Text = "pageEvents";

            pageSpouses.Location = new Point(4, 22);
            pageSpouses.Size = new Size(691, 230);
            pageSpouses.Text = "pageSpouses";

            pageNames.BackgroundColor = SystemColors.Control;
            pageNames.Location = new Point(4, 22);
            pageNames.Size = new Size(691, 230);
            pageNames.Text = "pageNames";

            pageAssociations.Location = new Point(4, 22);
            pageAssociations.Size = new Size(691, 230);
            pageAssociations.Text = "pageAssociations";

            pageGroups.Location = new Point(4, 22);
            pageGroups.Size = new Size(691, 230);
            pageGroups.Text = "pageGroups";

            pageNotes.Location = new Point(4, 22);
            pageNotes.Size = new Size(691, 230);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Location = new Point(4, 22);
            pageMultimedia.Size = new Size(691, 230);
            pageMultimedia.Text = "pageMultimedia";

            pageSources.Location = new Point(4, 22);
            pageSources.Size = new Size(691, 230);
            pageSources.Text = "pageSources";

            pageUserRefs.Location = new Point(4, 22);
            pageUserRefs.Size = new Size(691, 230);
            pageUserRefs.Text = "pageUserRefs";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(699, 563);
            Controls.Add(tabsPersonData);
            Controls.Add(lblRestriction);
            Controls.Add(btnNameCopy);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(cmbRestriction);
            Controls.Add(GroupBox1);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "PersonEditDlg";
            GroupBox1.ResumeLayout();
            panCtlParents.ResumeLayout();
            tabsPersonData.ResumeLayout();
            ResumeLayout();
        }
    }
}
