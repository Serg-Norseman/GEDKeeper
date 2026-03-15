#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class FamilyEditDlg
    {
        private TabView tabsData;
        private TabView.Tab pageEvents;
        private TabView.Tab pageNotes;
        private TabView.Tab pageMultimedia;
        private TabView.Tab pageSources;
        private TabView.Tab pageChilds;
        private Button btnAccept;
        private Button btnCancel;
        private FrameView frameFamily;
        private Label lblHusband;
        private TextField txtHusband;
        private Button btnHusbandAdd;
        private Button btnHusbandDelete;
        private Button btnHusbandSel;
        private Button btnWifeSel;
        private Button btnWifeDelete;
        private Button btnWifeAdd;
        private TextField txtWife;
        private Label lblWife;
        private Label lblStatus;
        private ComboBox cmbMarriageStatus;
        private Label lblRestriction;
        private ComboBox cmbRestriction;
        private TabView.Tab pageUserRefs;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            frameFamily = new FrameView();
            lblHusband = new Label();
            btnHusbandAdd = new Button();
            btnHusbandDelete = new Button();
            btnHusbandSel = new Button();
            btnWifeSel = new Button();
            btnWifeDelete = new Button();
            btnWifeAdd = new Button();
            lblWife = new Label();
            lblStatus = new Label();
            txtHusband = new TextField();
            txtWife = new TextField();
            cmbMarriageStatus = new ComboBox();
            lblRestriction = new Label();
            cmbRestriction = new ComboBox();
            tabsData = new TabView();
            pageChilds = new TabView.Tab();
            pageEvents = new TabView.Tab();
            pageNotes = new TabView.Tab();
            pageMultimedia = new TabView.Tab();
            pageSources = new TabView.Tab();
            pageUserRefs = new TabView.Tab();

            btnAccept.Width = 16;
            btnAccept.TabIndex = 4;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Width = 16;
            btnCancel.TabIndex = 5;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblHusband.X = 1;
            lblHusband.Y = 1;
            lblHusband.Width = 10;
            lblHusband.TabIndex = 0;

            txtHusband.X = 14;
            txtHusband.Y = 1;
            txtHusband.Width = 60;
            txtHusband.ReadOnly = true;
            txtHusband.TabIndex = 1;
            txtHusband.TextChanged += EditSpouse_TextChanged;

            btnHusbandAdd.Enabled = false;
            btnHusbandAdd.X = 76;
            btnHusbandAdd.Y = 1;
            btnHusbandAdd.Width = 7;
            btnHusbandAdd.TabIndex = 2;
            btnHusbandAdd.Clicked += btnHusbandAddClick;
            btnHusbandAdd.Text = "+";

            btnHusbandDelete.Enabled = false;
            btnHusbandDelete.X = 84;
            btnHusbandDelete.Y = 1;
            btnHusbandDelete.Width = 7;
            btnHusbandDelete.TabIndex = 3;
            btnHusbandDelete.Clicked += btnHusbandDeleteClick;
            btnHusbandDelete.Text = "-";

            btnHusbandSel.X = 92;
            btnHusbandSel.Y = 1;
            btnHusbandSel.Width = 7;
            btnHusbandSel.TabIndex = 4;
            btnHusbandSel.Clicked += btnHusbandSelClick;
            btnHusbandSel.Text = ">";

            lblWife.X = 1;
            lblWife.Y = 3;
            lblWife.Width = 6;
            lblWife.TabIndex = 5;

            txtWife.X = 14;
            txtWife.Y = 3;
            txtWife.Width = 60;
            txtWife.ReadOnly = true;
            txtWife.TabIndex = 6;
            txtWife.TextChanged += EditSpouse_TextChanged;

            btnWifeAdd.Enabled = false;
            btnWifeAdd.X = 76;
            btnWifeAdd.Y = 3;
            btnWifeAdd.Width = 7;
            btnWifeAdd.TabIndex = 7;
            btnWifeAdd.Clicked += btnWifeAddClick;
            btnWifeAdd.Text = "+";

            btnWifeDelete.Enabled = false;
            btnWifeDelete.X = 84;
            btnWifeDelete.Y = 3;
            btnWifeDelete.Width = 7;
            btnWifeDelete.TabIndex = 8;
            btnWifeDelete.Clicked += btnWifeDeleteClick;
            btnWifeDelete.Text = "-";

            btnWifeSel.X = 92;
            btnWifeSel.Y = 3;
            btnWifeSel.Width = 7;
            btnWifeSel.TabIndex = 9;
            btnWifeSel.Clicked += btnWifeSelClick;
            btnWifeSel.Text = ">";

            lblStatus.X = 1;
            lblStatus.Y = 5;
            lblStatus.Width = 8;
            lblStatus.TabIndex = 10;
            lblStatus.Text = "lblStatus";

            cmbMarriageStatus.X = 14;
            cmbMarriageStatus.Y = 5;
            cmbMarriageStatus.Width = 30;
            cmbMarriageStatus.TabIndex = 11;

            frameFamily.Add(lblHusband);
            frameFamily.Add(btnHusbandAdd);
            frameFamily.Add(btnHusbandDelete);
            frameFamily.Add(btnHusbandSel);
            frameFamily.Add(btnWifeSel);
            frameFamily.Add(btnWifeDelete);
            frameFamily.Add(btnWifeAdd);
            frameFamily.Add(lblWife);
            frameFamily.Add(lblStatus);
            frameFamily.Add(txtHusband);
            frameFamily.Add(txtWife);
            frameFamily.Add(cmbMarriageStatus);
            frameFamily.X = 0;
            frameFamily.Y = 0;
            frameFamily.Width = Dim.Fill();
            frameFamily.Height = 9;
            frameFamily.TabIndex = 0;
            frameFamily.Title = "";

            tabsData.AddTab(pageChilds, true);
            tabsData.AddTab(pageEvents, false);
            tabsData.AddTab(pageNotes, false);
            tabsData.AddTab(pageMultimedia, false);
            tabsData.AddTab(pageSources, false);
            tabsData.AddTab(pageUserRefs, false);
            tabsData.X = 0;
            tabsData.Y = 9;
            tabsData.Width = Dim.Fill();
            tabsData.Height = 26;
            tabsData.TabIndex = 1;

            lblRestriction.X = 1;
            lblRestriction.Y = 36;
            lblRestriction.Width = 11;
            lblRestriction.TabIndex = 2;

            cmbRestriction.X = 28;
            cmbRestriction.Y = 36;
            cmbRestriction.Width = 25;
            cmbRestriction.TabIndex = 3;
            cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

            Width = 104;
            Height = 39;

            Add(tabsData);
            Add(frameFamily);

            Add(lblRestriction);
            Add(cmbRestriction);
        }
    }
}
