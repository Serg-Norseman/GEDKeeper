#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class MediaEditDlg
    {
        private TabView tabsData;
        private TabPage pageNotes;
        private TabPage pageSources;
        private Button btnAccept;
        private Button btnCancel;
        private Button btnView;
        private TabPage pageCommon;
        private Label lblName;
        private TextField txtName;
        private Label lblType;
        private ComboBox cmbMediaType;
        private Label lblStoreType;
        private ComboBox cmbStoreType;
        private Label lblFile;
        private TextField txtFile;
        private Button btnFileSelect;
        private TabPage pageUserRefs;
        private TabPage pageFiles;

        private void InitializeComponent()
        {
            tabsData = new TabView();
            pageCommon = new TabPage();
            lblName = new Label();
            lblType = new Label();
            lblStoreType = new Label();
            lblFile = new Label();
            txtName = new TextField();
            cmbMediaType = new ComboBox();
            cmbStoreType = new ComboBox();
            txtFile = new TextField();
            btnFileSelect = new Button();
            pageNotes = new TabPage();
            pageSources = new TabPage();
            btnAccept = new Button();
            btnCancel = new Button();
            btnView = new Button();
            pageUserRefs = new TabPage();
            pageFiles = new TabPage();

            btnView.Size = new Size(16, 1);
            btnView.TabIndex = 1;
            btnView.Clicked += btnView_Click;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnView);
            AddButton(btnAccept);
            AddButton(btnCancel);

            lblName.Location = new Point(1, 1);
            lblName.TabIndex = 0;

            txtName.Location = new Point(12, 1);
            txtName.Size = new Size(55, 1);
            txtName.TabIndex = 1;
            txtName.TextChanged += edName_TextChanged;

            lblFile.Location = new Point(1, 3);
            lblFile.TabIndex = 2;

            txtFile.Location = new Point(12, 3);
            txtFile.ReadOnly = true;
            txtFile.Size = new Size(46, 1);
            txtFile.TabIndex = 3;

            btnFileSelect.Text = "...";
            btnFileSelect.Location = new Point(59, 3);
            btnFileSelect.Size = new Size(10, 1);
            btnFileSelect.TabIndex = 4;
            btnFileSelect.Clicked += btnFileSelect_Click;

            lblType.Location = new Point(1, 5);
            lblType.TabIndex = 5;

            cmbMediaType.Location = new Point(12, 5);
            cmbMediaType.Size = new Size(20, 2);
            cmbMediaType.TabIndex = 6;

            lblStoreType.Location = new Point(1, 7);
            lblStoreType.TabIndex = 7;

            cmbStoreType.Location = new Point(20, 7);
            cmbStoreType.Size = new Size(36, 2);
            cmbStoreType.TabIndex = 8;
            cmbStoreType.SelectedIndexChanged += cmbStoreType_SelectedIndexChanged;

            var pageComCont = new View();
            pageComCont.Add(lblName);
            pageComCont.Add(lblType);
            pageComCont.Add(lblStoreType);
            pageComCont.Add(lblFile);
            pageComCont.Add(txtName);
            pageComCont.Add(cmbMediaType);
            pageComCont.Add(cmbStoreType);
            pageComCont.Add(txtFile);
            pageComCont.Add(btnFileSelect);
            pageCommon.View = pageComCont;

            tabsData.AddTab(pageCommon, true);
            tabsData.AddTab(pageFiles, false);
            tabsData.AddTab(pageNotes, false);
            tabsData.AddTab(pageSources, false);
            tabsData.AddTab(pageUserRefs, false);
            tabsData.Location = new Point(0, 0);
            tabsData.Size = new Size(70, 32);
            tabsData.TabIndex = 0;

            Size = new Size(72, 36);
            Add(tabsData);
        }
    }
}
