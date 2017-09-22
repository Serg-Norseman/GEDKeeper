using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class LocationEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageCommon;
        private Label lblName;
        private TextBox txtName;
        private Label lblLatitude;
        private TextBox txtLatitude;
        private Label lblLongitude;
        private TextBox txtLongitude;
        private GroupBox grpSearch;
        private GKListView ListGeoCoords;
        private Button btnSearch;
        private Button btnSelect;
        private Button btnSelectName;
        private Button btnShowOnMap;
        private Panel panMap;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblName = new Label();
            lblName.Text = "lblName";

            lblLatitude = new Label();
            lblLatitude.Text = "lblLatitude";

            lblLongitude = new Label();
            lblLongitude.Text = "lblLongitude";

            txtName = new TextBox();
            txtName.TextChanged += EditName_TextChanged;
            txtName.KeyDown += EditName_KeyDown;

            txtLatitude = new TextBox();

            txtLongitude = new TextBox();

            ListGeoCoords = new GKListView();
            ListGeoCoords.MouseDown += ListGeoCoords_Click;
            ListGeoCoords.AddColumn("Name", 200);
            ListGeoCoords.AddColumn("Latitude", 80);
            ListGeoCoords.AddColumn("Longitude", 80);

            btnSearch = new Button();
            btnSearch.Size = new Size(130, 26);
            btnSearch.Text = "btnSearch";
            btnSearch.Click += btnSearch_Click;

            btnSelect = new Button();
            btnSelect.Size = new Size(130, 26);
            btnSelect.Text = "btnSelect";
            btnSelect.Click += btnSelect_Click;

            btnSelectName = new Button();
            btnSelectName.Size = new Size(130, 26);
            btnSelectName.Text = "btnSelectName";
            btnSelectName.Click += btnSelectName_Click;

            btnShowOnMap = new Button();
            btnShowOnMap.Size = new Size(130, 26);
            btnShowOnMap.Text = "btnShowOnMap";
            btnShowOnMap.Click += btnShowOnMap_Click;

            panMap = new Panel();
            //panMap.BorderStyle = BorderStyle.Fixed3D;

            grpSearch = new GroupBox();
            grpSearch.Text = "grpSearch";
            grpSearch.Content = new TableLayout {
                Rows = {
                    new TableRow {
                        Cells = { new TableLayout {
                                Spacing = new Size(10, 10),
                                Rows = {
                                    new TableRow {
                                        Cells = {
                                            ListGeoCoords,
                                            new DefStackLayout(Orientation.Vertical, 10, btnSearch, btnSelect, btnSelectName)
                                        }
                                    }
                                }
                            } }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panMap }
                    }
                }
            };

            pageCommon = new TabPage();
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Rows = {
                    new TableRow {
                        Cells = { new DefTableLayout {
                                Rows = {
                                    new TableRow {
                                        Cells = { lblName, lblLatitude, lblLongitude, null }
                                    },
                                    new TableRow {
                                        Cells = { txtName, txtLatitude, txtLongitude, new TableCell(btnShowOnMap, false) }
                                    }
                                }
                            } }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpSearch }
                    }
                }
            };

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            pageMultimedia = new TabPage();
            pageMultimedia.Text = "pageMultimedia";

            tabsData = new TabControl();
            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageMultimedia);

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
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "LocationEditDlg";

            SetPredefProperties(780, 580);
            ResumeLayout();
        }
    }
}
