using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

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

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(91, 24);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(91, 24);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            lblRestriction.Size = new Size(68, 13);
            lblRestriction.Text = "lblRestriction";

            cmbRestriction.ReadOnly = true;
            cmbRestriction.Size = new Size(163, 21);
            cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

            GroupBox1.Size = new Size(699, 258);
            var personLayout = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblSurname, lblSurnamePrefix }
                    },
                    new TableRow {
                        Cells = { txtSurname, txtSurnamePrefix }
                    },
                    new TableRow {
                        Cells = { lblMarriedSurname, lblNamePrefix, btnPortraitAdd }
                    },
                    new TableRow {
                        Cells = { txtMarriedSurname, txtNamePrefix, btnPortraitDelete }
                    },
                    new TableRow {
                        Cells = { lblName, lblNameSuffix, lblSex }
                    },
                    new TableRow {
                        Cells = { txtName, txtNameSuffix, cmbSex }
                    },
                    new TableRow {
                        Cells = { lblPatronymic, lblNickname, chkPatriarch }
                    },
                    new TableRow {
                        Cells = { cmbPatronymic, txtNickname, chkBookmark }
                    },
                    null
                }
            };
            GroupBox1.Content = new StackLayout {
                Orientation = Orientation.Horizontal,
                Items = { personLayout, imgPortrait }
            };

            imgPortrait.Cursor = Cursors.Arrow;
            imgPortrait.Image = null;
            imgPortrait.Size = new Size(149, 165);
            //imgPortrait.SizeMode = PictureBoxSizeMode.Normal;
            imgPortrait.SlidePanelHeight = 36;
            imgPortrait.PixelSpeed = 5;

            lblMarriedSurname.Size = new Size(95, 13);
            lblMarriedSurname.Text = "lblMarriedSurname";

            lblSurname.Size = new Size(59, 13);
            lblSurname.Text = "lblSurname";

            lblName.Size = new Size(44, 13);
            lblName.Text = "lblName";

            lblPatronymic.Size = new Size(70, 13);
            lblPatronymic.Text = "lblPatronymic";

            lblSex.Size = new Size(35, 13);
            lblSex.Text = "lblSex";

            lblSurnamePrefix.Size = new Size(87, 13);
            lblSurnamePrefix.Text = "lblSurnamePrefix";

            lblNamePrefix.Size = new Size(72, 13);
            lblNamePrefix.Text = "lblNamePrefix";

            lblNameSuffix.Size = new Size(72, 13);
            lblNameSuffix.Text = "lblNameSuffix";

            lblNickname.Size = new Size(62, 13);
            lblNickname.Text = "lblNickname";

            btnPortraitAdd.Size = new Size(29, 29);
            btnPortraitAdd.Click += btnPortraitAdd_Click;

            btnPortraitDelete.Size = new Size(29, 29);
            btnPortraitDelete.Click += btnPortraitDelete_Click;

            txtMarriedSurname.Size = new Size(182, 21);
            txtMarriedSurname.KeyDown += edSurname_KeyDown;
            //txtMarriedSurname.KeyPress += edSurname_KeyPress;

            txtSurname.Size = new Size(182, 21);
            txtSurname.KeyDown += edSurname_KeyDown;
            //txtSurname.KeyPress += edSurname_KeyPress;

            txtName.Size = new Size(182, 21);
            txtName.KeyDown += edSurname_KeyDown;
            //txtName.KeyPress += edSurname_KeyPress;

            cmbPatronymic.Size = new Size(182, 21);
            cmbPatronymic.KeyDown += edSurname_KeyDown;
            //cmbPatronymic.KeyPress += edSurname_KeyPress;

            cmbSex.ReadOnly = true;
            cmbSex.Size = new Size(154, 21);
            cmbSex.SelectedIndexChanged += cbSex_SelectedIndexChanged;

            chkPatriarch.Size = new Size(85, 17);
            chkPatriarch.Text = "chkPatriarch";

            //panCtlParents.BorderStyle = BorderStyle.FixedSingle;
            panCtlParents.Size = new Size(696, 69);
            panCtlParents.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblParents, txtFather, txtMother, btnParentsAdd, btnParentsEdit, btnParentsDelete }
                    },
                    new TableRow {
                        Cells = { btnFatherAdd, btnFatherDelete, btnFatherSel, btnMotherAdd, btnMotherDelete, btnMotherSel }
                    }
                }
            };

            txtMother.TextColor = SystemColors.Control;
            txtMother.ReadOnly = true;
            txtMother.Size = new Size(252, 21);

            lblParents.Size = new Size(54, 13);
            lblParents.Text = "lblParents";

            btnParentsAdd.Size = new Size(29, 29);
            btnParentsAdd.Click += btnParentsAdd_Click;

            btnParentsEdit.Size = new Size(29, 29);
            btnParentsEdit.Click += btnParentsEdit_Click;

            btnParentsDelete.Size = new Size(30, 29);
            btnParentsDelete.Click += btnParentsDelete_Click;

            btnFatherAdd.Size = new Size(29, 29);
            btnFatherAdd.Click += btnFatherAdd_Click;

            btnFatherDelete.Size = new Size(29, 29);
            btnFatherDelete.Click += btnFatherDelete_Click;

            btnFatherSel.Size = new Size(29, 29);
            btnFatherSel.Click += btnFatherSel_Click;

            btnMotherAdd.Size = new Size(29, 29);
            btnMotherAdd.Click += btnMotherAdd_Click;

            btnMotherDelete.Size = new Size(29, 29);
            btnMotherDelete.Click += btnMotherDelete_Click;

            btnMotherSel.Size = new Size(29, 29);
            btnMotherSel.Click += btnMotherSel_Click;

            txtFather.TextColor = SystemColors.Control;
            txtFather.ReadOnly = true;
            txtFather.Size = new Size(251, 21);

            chkBookmark.Size = new Size(88, 17);
            chkBookmark.Text = "chkBookmark";

            txtSurnamePrefix.Size = new Size(136, 21);

            txtNamePrefix.Size = new Size(136, 21);

            txtNameSuffix.Size = new Size(136, 21);

            txtNickname.Size = new Size(136, 21);

            btnNameCopy.Size = new Size(37, 24);
            btnNameCopy.Click += btnNameCopy_Click;

            tabsPersonData.Pages.Add(pageEvents);
            tabsPersonData.Pages.Add(pageSpouses);
            tabsPersonData.Pages.Add(pageNames);
            tabsPersonData.Pages.Add(pageAssociations);
            tabsPersonData.Pages.Add(pageGroups);
            tabsPersonData.Pages.Add(pageNotes);
            tabsPersonData.Pages.Add(pageMultimedia);
            tabsPersonData.Pages.Add(pageSources);
            tabsPersonData.Pages.Add(pageUserRefs);
            tabsPersonData.SelectedIndex = 0;
            tabsPersonData.Size = new Size(699, 256);

            pageEvents.Size = new Size(691, 230);
            pageEvents.Text = "pageEvents";

            pageSpouses.Size = new Size(691, 230);
            pageSpouses.Text = "pageSpouses";

            pageNames.BackgroundColor = SystemColors.Control;
            pageNames.Size = new Size(691, 230);
            pageNames.Text = "pageNames";

            pageAssociations.Size = new Size(691, 230);
            pageAssociations.Text = "pageAssociations";

            pageGroups.Size = new Size(691, 230);
            pageGroups.Text = "pageGroups";

            pageNotes.Size = new Size(691, 230);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Size = new Size(691, 230);
            pageMultimedia.Text = "pageMultimedia";

            pageSources.Size = new Size(691, 230);
            pageSources.Text = "pageSources";

            pageUserRefs.Size = new Size(691, 230);
            pageUserRefs.Text = "pageUserRefs";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { GroupBox1 }
                    },
                    new TableRow {
                        Cells = { panCtlParents }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsPersonData }
                    },
                    new TableRow {
                        Cells = { lblRestriction, cmbRestriction, btnNameCopy, null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(699, 563);
            Title = "PersonEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
