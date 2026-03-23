#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class PersonEditDlg
    {
        private TabView tabsData;
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
        private FrameView grpFields;
        private Label lblSurname;
        private Label lblName;
        private Label lblPatronymic;
        private Label lblSex;
        private TextField txtSurname;
        private TextField txtName;
        private ComboBox cmbPatronymic;
        public ComboBox cmbSex;
        private CheckBox chkPatriarch;
        private TabPage pageUserRefs;
        private FrameView panParents;
        private Label lblParents;
        private TextField txtFather;
        private TextField txtMother;
        private Button btnParentsAdd;
        private Button btnParentsEdit;
        private Button btnParentsDelete;
        private CheckBox chkBookmark;
        private Label lblNickname;
        private TextField txtNickname;
        private Button btnNameCopy;
        private View imgPortrait;
        private Button btnPortraitAdd;
        private Button btnPortraitDelete;
        private Button btnFatherAdd;
        private Button btnFatherDelete;
        private Button btnFatherSel;
        private Button btnMotherAdd;
        private Button btnMotherDelete;
        private Button btnMotherSel;
        private TabPage pageNames;
        private TextField txtMarriedSurname;
        private Label lblMarriedSurname;
        private TabPage pageParents;
        private TabPage pageChilds;
        private TabPage pageDNATests;
        private TabPage pageFamily;
        private TabView tabsFamily;
        private TabPage pageOther;
        private TabView tabsOther;
        private FrameView panPortrait;
        private Label lblMother;
        private Label lblFather;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            lblRestriction = new Label();
            cmbRestriction = new ComboBox();
            grpFields = new FrameView();
            lblMarriedSurname = new Label();
            lblSurname = new Label();
            lblName = new Label();
            lblPatronymic = new Label();
            lblSex = new Label();
            lblNickname = new Label();
            txtMarriedSurname = new TextField();
            txtSurname = new TextField();
            txtName = new TextField();
            cmbPatronymic = new ComboBox();
            cmbSex = new ComboBox();
            chkPatriarch = new CheckBox();
            chkBookmark = new CheckBox();
            txtNickname = new TextField();
            imgPortrait = new View();
            btnPortraitAdd = new Button();
            btnPortraitDelete = new Button();
            panParents = new FrameView();
            txtMother = new TextField();
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
            txtFather = new TextField();
            btnNameCopy = new Button();
            tabsData = new TabView();
            pageEvents = new TabPage();
            pageFamily = new TabPage();
            tabsFamily = new TabView();
            pageSpouses = new TabPage();
            pageParents = new TabPage();
            pageChilds = new TabPage();
            pageAssociations = new TabPage();
            pageNames = new TabPage();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            pageSources = new TabPage();
            pageOther = new TabPage();
            tabsOther = new TabView();
            pageUserRefs = new TabPage();
            pageGroups = new TabPage();
            pageDNATests = new TabPage();
            panPortrait = new FrameView();
            lblFather = new Label();
            lblMother = new Label();

            btnNameCopy.Text = "->";
            btnNameCopy.Size = new Size(16, 1);
            btnNameCopy.TabIndex = 52;
            btnNameCopy.Clicked += btnNameCopy_Click;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 53;
            btnAccept.Clicked += AcceptClickHandler;
            btnAccept.IsDefault = true;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 54;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnNameCopy);
            AddButton(btnAccept);
            AddButton(btnCancel);

            lblRestriction.Location = new Point(1, 46);
            lblRestriction.TabIndex = 50;

            cmbRestriction.Location = new Point(26, 46);
            cmbRestriction.Size = new Size(20, 2);
            cmbRestriction.TabIndex = 51;
            cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

            Add(lblRestriction, cmbRestriction);

            grpFields.Add(lblMarriedSurname);
            grpFields.Add(lblSurname);
            grpFields.Add(lblName);
            grpFields.Add(lblPatronymic);
            grpFields.Add(lblSex);
            grpFields.Add(lblNickname);
            grpFields.Add(txtMarriedSurname);
            grpFields.Add(txtSurname);
            grpFields.Add(txtName);
            grpFields.Add(cmbPatronymic);
            grpFields.Add(cmbSex);
            grpFields.Add(chkPatriarch);
            grpFields.Add(chkBookmark);
            grpFields.Add(txtNickname);
            grpFields.Location = new Point(0, 0);
            grpFields.Size = new Size(78, 11);
            grpFields.TabIndex = 0;

            lblSurname.Location = new Point(1, 1);
            lblSurname.TabIndex = 1;

            lblMarriedSurname.Location = new Point(1, 3);
            lblMarriedSurname.TabIndex = 3;

            lblName.Location = new Point(1, 5);
            lblName.TabIndex = 5;

            lblPatronymic.Location = new Point(1, 7);
            lblPatronymic.TabIndex = 7;

            lblNickname.Location = new Point(44, 1);
            lblNickname.TabIndex = 9;

            lblSex.Location = new Point(44, 3);
            lblSex.TabIndex = 11;

            txtSurname.Location = new Point(17, 1);
            txtSurname.Size = new Size(25, 1);
            txtSurname.TabIndex = 2;
            txtSurname.KeyDown += txtXName_KeyDown;
            txtSurname.KeyPress += edSurname_KeyPress;
            txtSurname.Leave += txtXName_Leave;

            txtMarriedSurname.Location = new Point(17, 3);
            txtMarriedSurname.Size = new Size(25, 1);
            txtMarriedSurname.TabIndex = 4;
            txtMarriedSurname.KeyDown += txtXName_KeyDown;
            txtMarriedSurname.KeyPress += edSurname_KeyPress;
            txtMarriedSurname.Leave += txtXName_Leave;

            txtName.Location = new Point(17, 5);
            txtName.Size = new Size(25, 1);
            txtName.TabIndex = 6;
            txtName.KeyDown += txtXName_KeyDown;
            txtName.KeyPress += edSurname_KeyPress;
            txtName.Leave += txtXName_Leave;

            cmbPatronymic.Location = new Point(17, 7);
            cmbPatronymic.Size = new Size(25, 2);
            cmbPatronymic.TabIndex = 8;
            cmbPatronymic.KeyDown += txtXName_KeyDown;
            cmbPatronymic.KeyPress += edSurname_KeyPress;
            cmbPatronymic.Leave += txtXName_Leave;

            txtNickname.Location = new Point(57, 1);
            txtNickname.Size = new Size(18, 1);
            txtNickname.TabIndex = 10;

            cmbSex.Location = new Point(57, 3);
            cmbSex.Size = new Size(18, 2);
            cmbSex.TabIndex = 12;
            cmbSex.SelectedIndexChanged += cbSex_SelectedIndexChanged;

            chkPatriarch.Location = new Point(57, 5);
            chkPatriarch.TabIndex = 13;

            chkBookmark.Location = new Point(57, 7);
            chkBookmark.TabIndex = 14;

            panParents.Add(lblMother);
            panParents.Add(lblFather);
            panParents.Add(txtMother);
            panParents.Add(lblParents);
            panParents.Add(btnParentsAdd);
            panParents.Add(btnParentsEdit);
            panParents.Add(btnParentsDelete);
            panParents.Add(btnFatherAdd);
            panParents.Add(btnFatherDelete);
            panParents.Add(btnFatherSel);
            panParents.Add(btnMotherAdd);
            panParents.Add(btnMotherDelete);
            panParents.Add(btnMotherSel);
            panParents.Add(txtFather);
            panParents.Location = new Point(0, 11);
            panParents.Size = new Size(78, 9);
            panParents.TabIndex = 20;

            lblParents.Location = new Point(1, 1);
            lblParents.TabIndex = 21;

            btnParentsAdd.Text = "+";
            btnParentsAdd.Location = new Point(58, 1);
            btnParentsAdd.Size = new Size(5, 1);
            btnParentsAdd.TabIndex = 22;
            btnParentsAdd.Clicked += btnParentsAdd_Click;

            btnParentsEdit.Text = "~";
            btnParentsEdit.Location = new Point(64, 1);
            btnParentsEdit.Size = new Size(5, 1);
            btnParentsEdit.TabIndex = 23;
            btnParentsEdit.Clicked += btnParentsEdit_Click;

            btnParentsDelete.Text = "x";
            btnParentsDelete.Location = new Point(70, 1);
            btnParentsDelete.Size = new Size(5, 1);
            btnParentsDelete.TabIndex = 24;
            btnParentsDelete.Clicked += btnParentsDelete_Click;

            lblFather.Location = new Point(1, 3);
            lblFather.TabIndex = 25;

            txtFather.Location = new Point(10, 3);
            txtFather.ReadOnly = true;
            txtFather.Size = new Size(47, 1);
            txtFather.TabIndex = 26;

            btnFatherAdd.Text = "+";
            btnFatherAdd.Location = new Point(58, 3);
            btnFatherAdd.Size = new Size(5, 1);
            btnFatherAdd.TabIndex = 27;
            btnFatherAdd.Clicked += btnFatherAdd_Click;

            btnFatherDelete.Text = "x";
            btnFatherDelete.Location = new Point(64, 3);
            btnFatherDelete.Size = new Size(5, 1);
            btnFatherDelete.TabIndex = 28;
            btnFatherDelete.Clicked += btnFatherDelete_Click;

            btnFatherSel.Text = ">";
            btnFatherSel.Location = new Point(70, 3);
            btnFatherSel.Size = new Size(5, 1);
            btnFatherSel.TabIndex = 29;
            btnFatherSel.Clicked += btnFatherSel_Click;

            lblMother.Location = new Point(1, 5);
            lblMother.TabIndex = 30;

            txtMother.Location = new Point(10, 5);
            txtMother.ReadOnly = true;
            txtMother.Size = new Size(47, 1);
            txtMother.TabIndex = 31;

            btnMotherAdd.Text = "+";
            btnMotherAdd.Location = new Point(58, 5);
            btnMotherAdd.Size = new Size(5, 1);
            btnMotherAdd.TabIndex = 32;
            btnMotherAdd.Clicked += btnMotherAdd_Click;

            btnMotherDelete.Text = "x";
            btnMotherDelete.Location = new Point(64, 5);
            btnMotherDelete.Size = new Size(5, 1);
            btnMotherDelete.TabIndex = 33;
            btnMotherDelete.Clicked += btnMotherDelete_Click;

            btnMotherSel.Text = ">";
            btnMotherSel.Location = new Point(70, 5);
            btnMotherSel.Size = new Size(5, 1);
            btnMotherSel.TabIndex = 34;
            btnMotherSel.Clicked += btnMotherSel_Click;

            //imgPortrait.Height = Dim.Fill();
            //imgPortrait.Width = Dim.Fill();
            imgPortrait.Location = new Point(8, 8);
            //imgPortrait.Size = new Size(142, 199);
            imgPortrait.TabIndex = 41;

            btnPortraitAdd.Text = "+";
            btnPortraitAdd.Location = new Point(45, 226);
            btnPortraitAdd.Size = new Size(5, 1);
            btnPortraitAdd.TabIndex = 42;
            btnPortraitAdd.TabStop = false;
            btnPortraitAdd.Clicked += btnPortraitAdd_Click;

            btnPortraitDelete.Text = "x";
            btnPortraitDelete.Location = new Point(82, 226);
            btnPortraitDelete.Size = new Size(5, 1);
            btnPortraitDelete.TabIndex = 43;
            btnPortraitDelete.TabStop = false;
            btnPortraitDelete.Clicked += btnPortraitDelete_Click;

            panPortrait.Add(imgPortrait);
            panPortrait.Add(btnPortraitAdd);
            panPortrait.Add(btnPortraitDelete);
            panPortrait.Location = new Point(78, 0);
            panPortrait.Size = new Size(36, 20);
            panPortrait.TabIndex = 40;

            var panFamily = new View();
            panFamily.Add(tabsFamily);
            tabsFamily.AddTab(pageSpouses);
            tabsFamily.AddTab(pageParents);
            tabsFamily.AddTab(pageChilds);
            tabsFamily.AddTab(pageAssociations);
            tabsFamily.Height = Dim.Fill();
            tabsFamily.Width = Dim.Fill();
            pageFamily.View = panFamily;

            var panOther = new View();
            panOther.Add(tabsOther);
            tabsOther.AddTab(pageUserRefs);
            tabsOther.AddTab(pageGroups);
            tabsOther.AddTab(pageDNATests);
            tabsOther.Height = Dim.Fill();
            tabsOther.Width = Dim.Fill();
            pageOther.View = panOther;

            tabsData.AddTab(pageEvents);
            tabsData.AddTab(pageFamily);
            tabsData.AddTab(pageNames);
            tabsData.AddTab(pageNotes);
            tabsData.AddTab(pageMultimedia);
            tabsData.AddTab(pageSources);
            tabsData.AddTab(pageOther);
            tabsData.Location = new Point(0, 20);
            tabsData.Height = 25;
            tabsData.Width = 114;
            tabsData.TabIndex = 50;

            Add(grpFields);
            Add(panParents);
            Add(panPortrait);
            Add(tabsData);

            Width = 116;
            Height = 49;
        }
    }
}
