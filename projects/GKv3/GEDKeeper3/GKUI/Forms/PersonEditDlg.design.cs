using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
        private TabPage pageParents;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblMarriedSurname = new Label();
            lblMarriedSurname.Text = "lblMarriedSurname";

            lblSurname = new Label();
            lblSurname.Text = "lblSurname";

            lblName = new Label();
            lblName.Text = "lblName";

            lblPatronymic = new Label();
            lblPatronymic.Text = "lblPatronymic";

            lblSex = new Label();
            lblSex.Text = "lblSex";

            lblSurnamePrefix = new Label();
            lblSurnamePrefix.Text = "lblSurnamePrefix";

            lblNamePrefix = new Label();
            lblNamePrefix.Text = "lblNamePrefix";

            lblNameSuffix = new Label();
            lblNameSuffix.Text = "lblNameSuffix";

            lblNickname = new Label();
            lblNickname.Text = "lblNickname";

            txtMarriedSurname = new TextBox();
            txtMarriedSurname.Size = new Size(180, 22);
            txtMarriedSurname.KeyDown += edNameX_KeyDown;

            txtSurname = new TextBox();
            txtSurname.Size = new Size(180, 22);
            txtSurname.KeyDown += edNameX_KeyDown;

            txtName = new TextBox();
            txtName.Size = new Size(180, 22);
            txtName.KeyDown += edNameX_KeyDown;

            cmbPatronymic = new ComboBox();
            cmbPatronymic.Size = new Size(180, 22);
            cmbPatronymic.KeyDown += edNameX_KeyDown;

            cmbSex = new ComboBox();
            cmbSex.ReadOnly = true;
            cmbSex.Size = new Size(180, 22);
            cmbSex.SelectedIndexChanged += cbSex_SelectedIndexChanged;

            chkPatriarch = new CheckBox();
            chkPatriarch.Text = "chkPatriarch";

            chkBookmark = new CheckBox();
            chkBookmark.Text = "chkBookmark";

            txtSurnamePrefix = new TextBox();
            txtSurnamePrefix.Size = new Size(180, 22);
            txtSurnamePrefix.KeyDown += edNameX_KeyDown;

            txtNamePrefix = new TextBox();
            txtNamePrefix.Size = new Size(180, 22);
            txtNamePrefix.KeyDown += edNameX_KeyDown;

            txtNameSuffix = new TextBox();
            txtNameSuffix.Size = new Size(180, 22);
            txtNameSuffix.KeyDown += edNameX_KeyDown;

            txtNickname = new TextBox();
            txtNickname.Size = new Size(180, 22);
            txtNickname.KeyDown += edNameX_KeyDown;

            var personLayout = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { new VSDefStackLayout(lblSurname, txtSurname),
                                  new VSDefStackLayout(lblSurnamePrefix, txtSurnamePrefix), null }
                    },
                    new TableRow {
                        Cells = { new VSDefStackLayout(lblMarriedSurname, txtMarriedSurname),
                                  new VSDefStackLayout(lblNamePrefix, txtNamePrefix), null }
                    },
                    new TableRow {
                        Cells = { new VSDefStackLayout(lblName, txtName),
                                  new VSDefStackLayout(lblNameSuffix, txtNameSuffix),
                                  new VSDefStackLayout(lblSex, cmbSex) }
                    },
                    new TableRow {
                        Cells = { new VSDefStackLayout(lblPatronymic, cmbPatronymic),
                                  new VSDefStackLayout(lblNickname, txtNickname),
                                  TableLayout.Horizontal(10, chkBookmark, chkPatriarch) }
                    }
                }
            };

            imgPortrait = new GKPortrait();
            imgPortrait.Image = null;
            imgPortrait.Width = 150;
            //imgPortrait.Size = new Size(149, 165);
            imgPortrait.SlidePanelHeight = 36;
            imgPortrait.PixelSpeed = 5;

            btnPortraitAdd = new Button();
            btnPortraitAdd.Size = UIHelper.ShortButtonSize;
            btnPortraitAdd.Click += btnPortraitAdd_Click;

            btnPortraitDelete = new Button();
            btnPortraitDelete.Size = UIHelper.ShortButtonSize;
            btnPortraitDelete.Click += btnPortraitDelete_Click;

            var portraitLayout = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { imgPortrait }
                    },
                    UIHelper.MakeDialogFooter(null, btnPortraitAdd, btnPortraitDelete, null)
                }
            };

            GroupBox1 = new GroupBox();
            GroupBox1.Content = TableLayout.Horizontal(10, new TableCell(personLayout, true), portraitLayout);

            //

            lblParents = new Label();
            lblParents.Text = "lblParents";

            txtFather = new TextBox();
            txtFather.Enabled = false;
            txtFather.Size = new Size(220, 22);

            txtMother = new TextBox();
            txtMother.Enabled = false;
            txtMother.Size = new Size(220, 22);

            btnParentsAdd = new Button();
            btnParentsAdd.Size = UIHelper.ShortButtonSize;
            btnParentsAdd.Click += btnParentsAdd_Click;

            btnParentsEdit = new Button();
            btnParentsEdit.Size = UIHelper.ShortButtonSize;
            btnParentsEdit.Click += btnParentsEdit_Click;

            btnParentsDelete = new Button();
            btnParentsDelete.Size = UIHelper.ShortButtonSize;
            btnParentsDelete.Click += btnParentsDelete_Click;

            btnFatherAdd = new Button();
            btnFatherAdd.Size = UIHelper.ShortButtonSize;
            btnFatherAdd.Click += btnFatherAdd_Click;

            btnFatherDelete = new Button();
            btnFatherDelete.Size = UIHelper.ShortButtonSize;
            btnFatherDelete.Click += btnFatherDelete_Click;

            btnFatherSel = new Button();
            btnFatherSel.Size = UIHelper.ShortButtonSize;
            btnFatherSel.Click += btnFatherSel_Click;

            btnMotherAdd = new Button();
            btnMotherAdd.Size = UIHelper.ShortButtonSize;
            btnMotherAdd.Click += btnMotherAdd_Click;

            btnMotherDelete = new Button();
            btnMotherDelete.Size = UIHelper.ShortButtonSize;
            btnMotherDelete.Click += btnMotherDelete_Click;

            btnMotherSel = new Button();
            btnMotherSel.Size = UIHelper.ShortButtonSize;
            btnMotherSel.Click += btnMotherSel_Click;

            var parentsTab = new DefTableLayout(4, 2);
            parentsTab.SetColumnScale(0, false);
            parentsTab.SetColumnScale(1, true);
            parentsTab.SetColumnScale(2, true);
            parentsTab.SetColumnScale(3, false);
            parentsTab.Add(lblParents, 0, 0);
            parentsTab.Add(txtFather, 1, 0);
            parentsTab.Add(txtMother, 2, 0);
            parentsTab.Add(new DefStackLayout(Orientation.Horizontal, 10, btnParentsAdd, btnParentsEdit, btnParentsDelete), 3, 0);
            parentsTab.Add(new DefStackLayout(Orientation.Horizontal, 10, btnFatherAdd, btnFatherDelete, btnFatherSel), 1, 1);
            parentsTab.Add(new DefStackLayout(Orientation.Horizontal, 10, btnMotherAdd, btnMotherDelete, btnMotherSel), 2, 1);

            panCtlParents = new GroupBox();
            panCtlParents.Content = parentsTab;

            //

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = UIHelper.LongButtonSize;
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = UIHelper.LongButtonSize;
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            //

            lblRestriction = new Label();
            lblRestriction.Text = "lblRestriction";

            cmbRestriction = new ComboBox();
            cmbRestriction.ReadOnly = true;
            cmbRestriction.Size = new Size(180, 22);
            cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

            btnNameCopy = new Button();
            btnNameCopy.Size = UIHelper.ShortButtonSize;
            btnNameCopy.Click += btnNameCopy_Click;

            //

            pageEvents = new TabPage();
            pageEvents.Text = "pageEvents";

            pageSpouses = new TabPage();
            pageSpouses.Text = "pageSpouses";

            pageNames = new TabPage();
            pageNames.Text = "pageNames";

            pageAssociations = new TabPage();
            pageAssociations.Text = "pageAssociations";

            pageGroups = new TabPage();
            pageGroups.Text = "pageGroups";

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            pageMultimedia = new TabPage();
            pageMultimedia.Text = "pageMultimedia";

            pageSources = new TabPage();
            pageSources.Text = "pageSources";

            pageUserRefs = new TabPage();
            pageUserRefs.Text = "pageUserRefs";

            pageParents = new TabPage();
            pageParents.Text = "pageParents";

            tabsPersonData = new TabControl();
            tabsPersonData.Pages.Add(pageEvents);
            tabsPersonData.Pages.Add(pageSpouses);
            tabsPersonData.Pages.Add(pageNames);
            tabsPersonData.Pages.Add(pageAssociations);
            tabsPersonData.Pages.Add(pageGroups);
            tabsPersonData.Pages.Add(pageNotes);
            tabsPersonData.Pages.Add(pageMultimedia);
            tabsPersonData.Pages.Add(pageSources);
            tabsPersonData.Pages.Add(pageUserRefs);
            tabsPersonData.Pages.Add(pageParents);
            //tabsPersonData.Size = new Size(600, 300);
            //tabsPersonData.Height = 200;

            Content = new DefTableLayout {
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
                    UIHelper.MakeDialogFooter(lblRestriction, cmbRestriction, null, btnNameCopy, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "PersonEditDlg";

            SetPredefProperties(700, 560);
            ResumeLayout();
        }
    }
}
