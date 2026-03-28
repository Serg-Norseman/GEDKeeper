#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class LocationEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabView tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageCommon;
        private Label lblName;
        private TextField txtName;
        private Label lblLatitude;
        private TextField txtLatitude;
        private Label lblLongitude;
        private TextField txtLongitude;
        private FrameView grpSearch;
        private GKListView ListGeoCoords;
        private Button btnSearch;
        private Button btnSelect;
        private Button btnSelectName;
        private TabPage pageHistory;
        private FrameView pageHistNames;
        private FrameView pageHistLinks;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageCommon = new TabPage();
            lblName = new Label();
            lblLatitude = new Label();
            lblLongitude = new Label();
            txtName = new TextField();
            txtLatitude = new TextField();
            txtLongitude = new TextField();
            grpSearch = new FrameView();
            ListGeoCoords = new GKListView();
            btnSearch = new Button();
            btnSelect = new Button();
            btnSelectName = new Button();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            pageHistory = new TabPage();
            pageHistNames = new FrameView();
            pageHistLinks = new FrameView();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 1;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 2;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            pageCommon.View.Add(lblName);
            pageCommon.View.Add(lblLatitude);
            pageCommon.View.Add(lblLongitude);
            pageCommon.View.Add(txtName);
            pageCommon.View.Add(txtLatitude);
            pageCommon.View.Add(txtLongitude);
            pageCommon.View.Add(grpSearch);

            lblName.Location = new Point(1, 1);
            lblName.TabIndex = 0;

            txtName.Location = new Point(1, 2);
            txtName.Size = new Size(53, 1);
            txtName.TabIndex = 1;
            //txtName.TextChanged += EditName_TextChanged;
            txtName.KeyDown += EditName_KeyDown;

            lblLatitude.Location = new Point(55, 1);
            lblLatitude.TabIndex = 2;

            txtLatitude.Location = new Point(55, 2);
            txtLatitude.Size = new Size(12, 1);
            txtLatitude.TabIndex = 3;

            lblLongitude.Location = new Point(68, 1);
            lblLongitude.TabIndex = 4;

            txtLongitude.Location = new Point(68, 2);
            txtLongitude.Size = new Size(12, 1);
            txtLongitude.TabIndex = 5;

            grpSearch.Add(ListGeoCoords);
            grpSearch.Add(btnSearch);
            grpSearch.Add(btnSelect);
            grpSearch.Add(btnSelectName);
            grpSearch.Location = new Point(1, 4);
            grpSearch.Size = new Size(80, 46);
            grpSearch.TabIndex = 7;
            grpSearch.TabStop = false;

            ListGeoCoords.Location = new Point(0, 3);
            ListGeoCoords.Width = Dim.Fill();
            ListGeoCoords.Height = Dim.Fill();
            ListGeoCoords.TabIndex = 1;
            //ListGeoCoords.Clicked += ListGeoCoords_Click;

            btnSearch.Location = new Point(1, 1);
            btnSearch.Size = new Size(16, 1);
            btnSearch.TabIndex = 2;
            btnSearch.Clicked += btnSearch_Click;

            btnSelect.Location = new Point(20, 1);
            btnSelect.Size = new Size(16, 1);
            btnSelect.TabIndex = 3;
            btnSelect.Clicked += btnSelect_Click;

            btnSelectName.Location = new Point(40, 1);
            btnSelectName.Size = new Size(16, 1);
            btnSelectName.TabIndex = 4;
            btnSelectName.Clicked += btnSelectName_Click;

            pageHistNames.Location = new Point(0, 0);
            pageHistNames.Height = Dim.Percent(50);
            pageHistNames.Width = Dim.Fill();
            pageHistNames.TabIndex = 0;

            pageHistLinks.X = 0;
            pageHistLinks.Y = Pos.Bottom(pageHistNames);
            pageHistLinks.Height = Dim.Percent(50);
            pageHistLinks.Width = Dim.Fill();
            pageHistLinks.TabIndex = 1;

            pageHistory.View.Add(pageHistNames);
            pageHistory.View.Add(pageHistLinks);

            tabsData.AddTab(pageCommon);
            tabsData.AddTab(pageHistory);
            tabsData.AddTab(pageNotes);
            tabsData.AddTab(pageMultimedia);
            tabsData.Location = new Point(0, 0);
            tabsData.Size = new Size(84, 40);
            tabsData.TabIndex = 0;

            Size = new Size(86, 44);
            Add(tabsData);
        }
    }
}
