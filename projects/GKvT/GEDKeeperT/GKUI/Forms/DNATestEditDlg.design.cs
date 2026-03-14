#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class DNATestEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabView tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageCommon;
        private Label lblTestName;
        private TextField txtTestName;
        private Label lblAgency;
        private ComboBox cmbAgency;
        private Label lblDate;
        private GKDateBox dateCtl;
        private Label lblFileRef;
        private TextField txtFileRef;
        private Label lblStoreType;
        private ComboBox cmbStoreType;
        private Button btnFileSelect;
        private Label lblFileFormat;
        private ComboBox cmbFileFormat;
        private Label lblYHaplogroup;
        private ComboBox cmbYHaplogroup;
        private Label lblMHaplogroup;
        private ComboBox cmbMHaplogroup;
        private Label lblRestriction;
        private ComboBox cmbRestriction;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageCommon = new TabPage();
            dateCtl = new GKDateBox();
            lblTestName = new Label();
            lblFileRef = new Label();
            lblDate = new Label();
            lblStoreType = new Label();
            lblAgency = new Label();
            txtTestName = new TextField();
            txtFileRef = new TextField();
            cmbStoreType = new ComboBox();
            cmbAgency = new ComboBox();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            btnFileSelect = new Button();
            lblFileFormat = new Label();
            cmbFileFormat = new ComboBox();
            lblMHaplogroup = new Label();
            cmbMHaplogroup = new ComboBox();
            lblYHaplogroup = new Label();
            cmbYHaplogroup = new ComboBox();
            lblRestriction = new Label();
            cmbRestriction = new ComboBox();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblTestName.Location = new Point(1, 1);
            lblTestName.TabIndex = 0;

            txtTestName.Location = new Point(1, 2);
            txtTestName.Size = new Size(40, 1);
            txtTestName.TabIndex = 1;

            lblDate.Location = new Point(42, 1);
            lblDate.TabIndex = 10;

            dateCtl.Location = new Point(42, 2);
            dateCtl.Size = new Size(11, 1);
            dateCtl.TabIndex = 10;

            lblAgency.Location = new Point(1, 4);
            lblAgency.TabIndex = 20;

            cmbAgency.Location = new Point(1, 5);
            cmbAgency.Size = new Size(55, 2);
            cmbAgency.TabIndex = 21;

            lblFileRef.Location = new Point(1, 7);
            lblFileRef.TabIndex = 5;

            txtFileRef.Location = new Point(1, 8);
            txtFileRef.Size = new Size(47, 1);
            txtFileRef.TabIndex = 6;

            btnFileSelect.Text = "...";
            btnFileSelect.Location = new Point(49, 8);
            btnFileSelect.Size = new Size(7, 1);
            btnFileSelect.TabIndex = 22;
            btnFileSelect.Clicked += btnFileSelect_Click;

            lblStoreType.Location = new Point(1, 10);
            lblStoreType.TabIndex = 18;

            cmbStoreType.Location = new Point(1, 11);
            cmbStoreType.Size = new Size(24, 2);
            cmbStoreType.TabIndex = 19;

            lblFileFormat.Location = new Point(26, 10);
            lblFileFormat.TabIndex = 18;

            cmbFileFormat.Location = new Point(26, 11);
            cmbFileFormat.Size = new Size(24, 2);
            cmbFileFormat.TabIndex = 19;

            lblMHaplogroup.Location = new Point(1, 13);
            lblMHaplogroup.TabIndex = 23;

            cmbMHaplogroup.Location = new Point(1, 14);
            cmbMHaplogroup.Size = new Size(55, 2);
            cmbMHaplogroup.TabIndex = 24;

            lblYHaplogroup.Location = new Point(1, 16);
            lblYHaplogroup.TabIndex = 25;

            cmbYHaplogroup.Location = new Point(1, 17);
            cmbYHaplogroup.Size = new Size(55, 2);
            cmbYHaplogroup.TabIndex = 26;

            lblRestriction.Location = new Point(1, 19);
            lblRestriction.TabIndex = 2;

            cmbRestriction.Location = new Point(1, 20);
            cmbRestriction.Size = new Size(20, 2);
            cmbRestriction.TabIndex = 3;
            cmbRestriction.SelectedItemChanged += cbRestriction_SelectedIndexChanged;

            var commView = new View();
            commView.Add(lblYHaplogroup);
            commView.Add(cmbYHaplogroup);
            commView.Add(lblMHaplogroup);
            commView.Add(cmbMHaplogroup);
            commView.Add(btnFileSelect);
            commView.Add(dateCtl);
            commView.Add(lblTestName);
            commView.Add(lblFileRef);
            commView.Add(lblDate);
            commView.Add(lblFileFormat);
            commView.Add(lblStoreType);
            commView.Add(lblAgency);
            commView.Add(txtTestName);
            commView.Add(txtFileRef);
            commView.Add(cmbFileFormat);
            commView.Add(cmbStoreType);
            commView.Add(cmbAgency);
            commView.Add(lblRestriction);
            commView.Add(cmbRestriction);
            pageCommon.View = commView;

            tabsData.AddTab(pageCommon);
            tabsData.AddTab(pageNotes);
            tabsData.AddTab(pageMultimedia);
            tabsData.Location = new Point(0, 0);
            tabsData.Size = new Size(59, 33);
            tabsData.TabIndex = 0;

            Size = new Size(61, 37);
            Add(tabsData);
        }
    }
}
