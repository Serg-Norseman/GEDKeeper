using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class MapsViewerWin
    {
        private TabControl PageControl1;
        private TabPage pagePlaces;
        private TreeView tvPlaces;
        private GroupBox grpSelection;
        private ComboBox cmbPersons;
        private CheckBox chkResidence;
        private CheckBox chkDeath;
        private CheckBox chkBirth;
        private Button btnSelectPlaces;
        private RadioButton radTotal;
        private RadioButton radSelected;
        private CheckBox chkLinesVisible;
        private ToolBar ToolBar1;
        private ButtonToolItem tbLoadPlaces;
        private ButtonToolItem tbSaveSnapshot;
        private ButtonToolItem tbProviders;
        private ContextMenu MenuProviders;
        private ButtonToolItem tbClear;
        private ButtonToolItem tbZoomCenter;
        private TabPage pageCoordinates;
        private Panel Panel1;
        private GroupBox gbCoords;
        private Label lblPlace;
        private TextBox txtPlace;
        private Label lblLng;
        private Label lblLat;
        private TextBox txtLng;
        private TextBox txtLat;
        private Button btnSearch;
        private Button btnAddRouteMarker;
        private Button btnAddPolygonMarker;
        private Slider trkZoom;
        private Button btnZoomUp;
        private Button btnZoomDown;
        private TableLayout panZoom;

        private void InitializeComponent()
        {
            SuspendLayout();

            MenuProviders = new ContextMenu();

            tbLoadPlaces = new ButtonToolItem();
            tbLoadPlaces.Text = "tbLoadPlaces";
            tbLoadPlaces.Click += tbLoadPlaces_Click;

            tbSaveSnapshot = new ButtonToolItem();
            tbSaveSnapshot.Click += tbSaveSnapshot_Click;

            tbProviders = new ButtonToolItem();
            tbProviders.Click += (sender, e) => MenuProviders.Show(this);

            tbClear = new ButtonToolItem();
            tbClear.Click += tbClear_Click;

            tbZoomCenter = new ButtonToolItem();
            tbZoomCenter.Text = "Zoom Center";
            tbZoomCenter.Click += tbZoomCenter_Click;

            ToolBar1 = new ToolBar();
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbLoadPlaces,
                                        new SeparatorToolItem(),
                                        tbSaveSnapshot,
                                        new SeparatorToolItem(),
                                        tbProviders,
                                        new SeparatorToolItem(),
                                        tbClear,
                                        tbZoomCenter});

            cmbPersons = new ComboBox();
            cmbPersons.ReadOnly = true;

            chkResidence = new CheckBox();
            chkResidence.Text = "chkResidence";

            chkDeath = new CheckBox();
            chkDeath.Text = "chkDeath";

            chkBirth = new CheckBox();
            chkBirth.Text = "chkBirth";

            radTotal = new RadioButton();
            radTotal.Text = "radTotal";
            radTotal.Click += radTotal_Click;

            radSelected = new RadioButton(radTotal);
            radSelected.Text = "radSelected";
            radSelected.Click += radTotal_Click;

            chkLinesVisible = new CheckBox();
            chkLinesVisible.Checked = true;
            chkLinesVisible.Text = "chkLinesVisible";

            btnSelectPlaces = new Button();
            btnSelectPlaces.Enabled = false;
            btnSelectPlaces.Text = "btnSelectPlaces";
            btnSelectPlaces.Click += btnSelectPlaces_Click;

            grpSelection = new GroupBox();
            grpSelection.Text = "grpSelection";
            grpSelection.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { radTotal }
                    },
                    new TableRow {
                        Cells = { chkBirth }
                    },
                    new TableRow {
                        Cells = { chkDeath }
                    },
                    new TableRow {
                        Cells = { chkResidence }
                    },
                    new TableRow {
                        Cells = { radSelected }
                    },
                    new TableRow {
                        Cells = { cmbPersons }
                    },
                    new TableRow {
                        Cells = { chkLinesVisible }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { TableLayout.Horizontal(10, btnSelectPlaces) }
                    }
                }
            };

            tvPlaces = new TreeView();
            tvPlaces.MouseDoubleClick += TreePlaces_DoubleClick;

            pagePlaces = new TabPage();
            pagePlaces.Text = "pagePlaces";
            pagePlaces.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { grpSelection }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tvPlaces }
                    }
                }
            };

            lblPlace = new Label();
            lblPlace.Text = "place";

            txtPlace = new TextBox();

            lblLng = new Label();
            lblLng.Text = "lng";

            txtLng = new TextBox();

            lblLat = new Label();
            lblLat.Text = "lat";

            txtLat = new TextBox();

            gbCoords = new GroupBox();
            gbCoords.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblPlace, txtPlace }
                    },
                    new TableRow {
                        Cells = { lblLat, txtLat }
                    },
                    new TableRow {
                        Cells = { lblLng, txtLng }
                    },
                }
            };

            btnSearch = new Button();
            btnSearch.Text = "Search";
            btnSearch.Click += btnSearch_Click;

            btnAddRouteMarker = new Button();
            btnAddRouteMarker.Text = "Add Route Marker";
            btnAddRouteMarker.Click += btnAddRouteMarker_Click;

            btnAddPolygonMarker = new Button();
            btnAddPolygonMarker.Text = "Add Polygon Marker";
            btnAddPolygonMarker.Click += btnAddPolygonMarker_Click;

            pageCoordinates = new TabPage();
            pageCoordinates.Content = new DefStackLayout(0, 10, Orientation.Vertical) {
                Items = {
                    gbCoords,
                    btnSearch,
                    btnAddRouteMarker,
                    btnAddPolygonMarker
                }
            };

            PageControl1 = new TabControl();
            PageControl1.Pages.Add(pagePlaces);
            PageControl1.Pages.Add(pageCoordinates);

            trkZoom = new Slider();
            trkZoom.Orientation = Orientation.Vertical;

            btnZoomUp = new Button();
            btnZoomUp.Text = "+";
            btnZoomUp.Click += btnZoomUp_Click;

            btnZoomDown = new Button();
            btnZoomDown.Text = "-";
            btnZoomDown.Click += btnZoomDown_Click;

            panZoom = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { btnZoomUp }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { trkZoom }
                    },
                    new TableRow {
                        Cells = { btnZoomDown }
                    },
                }
            };

            Panel1 = new Panel();

            ToolBar = ToolBar1;
            Content = TableLayout.Horizontal(4, PageControl1, new TableCell(Panel1, true), panZoom);

            Title = "MapsViewerWin";
            KeyDown += MapsViewerWin_KeyDown;

            UIHelper.SetPredefProperties(this, 1100, 570);
            ResumeLayout();
        }
    }
}
