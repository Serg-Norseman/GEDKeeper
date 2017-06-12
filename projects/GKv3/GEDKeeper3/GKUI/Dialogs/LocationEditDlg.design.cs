using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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
        private GKListViewStub ListGeoCoords;
        private Button btnSearch;
        private Button btnSelect;
        private Button btnSelectName;
        private Button btnShowOnMap;
        private Panel panMap;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageCommon = new TabPage();
            lblName = new Label();
            lblLatitude = new Label();
            lblLongitude = new Label();
            txtName = new TextBox();
            txtLatitude = new TextBox();
            txtLongitude = new TextBox();
            grpSearch = new GroupBox();
            ListGeoCoords = new GKListViewStub();
            btnSearch = new Button();
            btnSelect = new Button();
            btnSelectName = new Button();
            panMap = new Panel();
            btnShowOnMap = new Button();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(113, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(113, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageMultimedia);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(785, 516);

            pageCommon.Size = new Size(777, 486);
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblName, lblLatitude, lblLongitude, null }
                    },
                    new TableRow {
                        Cells = { txtName, txtLatitude, txtLongitude, btnShowOnMap }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpSearch }
                    }
                }
            };

            lblName.Size = new Size(55, 17);
            lblName.Text = "lblName";

            lblLatitude.Size = new Size(69, 17);
            lblLatitude.Text = "lblLatitude";

            lblLongitude.Size = new Size(81, 17);
            lblLongitude.Text = "lblLongitude";

            txtName.Size = new Size(371, 24);
            txtName.TextChanged += EditName_TextChanged;
            txtName.KeyDown += EditName_KeyDown;

            txtLatitude.Size = new Size(113, 24);

            txtLongitude.Size = new Size(113, 24);

            grpSearch.Size = new Size(774, 420);
            grpSearch.Text = "grpSearch";
            grpSearch.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { ListGeoCoords, new StackLayout {
                                Orientation = Orientation.Vertical,
                                Items = { btnSearch, btnSelect, btnSelectName }
                            } }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panMap }
                    }
                }
            };

            //ListGeoCoords.FullRowSelect = true;
            ListGeoCoords.Size = new Size(563, 109);
            //ListGeoCoords.View = View.Details;
            //ListGeoCoords.Click += ListGeoCoords_Click;

            /*ColumnHeader1.Text = "Name";
            ColumnHeader1.Width = 200;

            ColumnHeader2.Text = "Latitude";
            ColumnHeader2.Width = 80;

            ColumnHeader3.Text = "Longitude";
            ColumnHeader3.Width = 80;*/

            btnSearch.Size = new Size(147, 31);
            btnSearch.Text = "btnSearch";
            btnSearch.Click += btnSearch_Click;

            btnSelect.Size = new Size(147, 31);
            btnSelect.Text = "btnSelect";
            btnSelect.Click += btnSelect_Click;

            btnSelectName.Size = new Size(147, 31);
            btnSelectName.Text = "btnSelectName";
            btnSelectName.Click += btnSelectName_Click;

            //panMap.BorderStyle = BorderStyle.Fixed3D;
            panMap.Size = new Size(768, 283);

            btnShowOnMap.Size = new Size(98, 26);
            btnShowOnMap.Text = "btnShowOnMap";
            btnShowOnMap.Click += btnShowOnMap_Click;

            pageNotes.Size = new Size(777, 486);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Size = new Size(777, 486);
            pageMultimedia.Text = "pageMultimedia";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(784, 578);
            Title = "LocationEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
