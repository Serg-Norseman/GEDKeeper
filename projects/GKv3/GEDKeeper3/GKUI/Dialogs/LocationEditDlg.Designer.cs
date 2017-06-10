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
        //private ColumnHeader ColumnHeader1;
        //private ColumnHeader ColumnHeader2;
        //private ColumnHeader ColumnHeader3;

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
            ListGeoCoords = new GKListView();
            ColumnHeader1 = new ColumnHeader();
            ColumnHeader2 = new ColumnHeader();
            ColumnHeader3 = new ColumnHeader();
            btnSearch = new Button();
            btnSelect = new Button();
            btnSelectName = new Button();
            panMap = new Panel();
            btnShowOnMap = new Button();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            tabsData.SuspendLayout();
            pageCommon.SuspendLayout();
            grpSearch.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(538, 534);
            btnAccept.Size = new Size(113, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(661, 534);
            btnCancel.Size = new Size(113, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Controls.Add(pageCommon);
            tabsData.Controls.Add(pageNotes);
            tabsData.Controls.Add(pageMultimedia);
            tabsData.Location = new Point(0, 0);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(785, 516);

            pageCommon.Controls.Add(lblName);
            pageCommon.Controls.Add(lblLatitude);
            pageCommon.Controls.Add(lblLongitude);
            pageCommon.Controls.Add(txtName);
            pageCommon.Controls.Add(txtLatitude);
            pageCommon.Controls.Add(txtLongitude);
            pageCommon.Controls.Add(grpSearch);
            pageCommon.Controls.Add(btnShowOnMap);
            pageCommon.Location = new Point(4, 26);
            pageCommon.Size = new Size(777, 486);
            pageCommon.Text = "pageCommon";

            lblName.Location = new Point(22, 10);
            lblName.Size = new Size(55, 17);
            lblName.Text = "lblName";

            lblLatitude.Location = new Point(402, 10);
            lblLatitude.Size = new Size(69, 17);
            lblLatitude.Text = "lblLatitude";

            lblLongitude.Location = new Point(525, 10);
            lblLongitude.Size = new Size(81, 17);
            lblLongitude.Text = "lblLongitude";

            txtName.Location = new Point(22, 29);
            txtName.Size = new Size(371, 24);
            txtName.TextChanged += EditName_TextChanged;
            txtName.KeyDown += EditName_KeyDown;

            txtLatitude.Location = new Point(402, 29);
            txtLatitude.Size = new Size(113, 24);

            txtLongitude.Location = new Point(525, 29);
            txtLongitude.Size = new Size(113, 24);

            grpSearch.Controls.Add(ListGeoCoords);
            grpSearch.Controls.Add(btnSearch);
            grpSearch.Controls.Add(btnSelect);
            grpSearch.Controls.Add(btnSelectName);
            grpSearch.Controls.Add(panMap);
            grpSearch.Location = new Point(0, 62);
            grpSearch.Size = new Size(774, 420);
            grpSearch.Text = "grpSearch";

            ListGeoCoords.Columns.AddRange(new ColumnHeader[] {
                                               ColumnHeader1,
                                               ColumnHeader2,
                                               ColumnHeader3});
            ListGeoCoords.FullRowSelect = true;
            ListGeoCoords.Location = new Point(22, 19);
            ListGeoCoords.Size = new Size(563, 109);
            ListGeoCoords.UseCompatibleStateImageBehavior = false;
            ListGeoCoords.View = View.Details;
            ListGeoCoords.Click += ListGeoCoords_Click;

            ColumnHeader1.Text = "Name";
            ColumnHeader1.Width = 200;

            ColumnHeader2.Text = "Latitude";
            ColumnHeader2.Width = 80;

            ColumnHeader3.Text = "Longitude";
            ColumnHeader3.Width = 80;

            btnSearch.Location = new Point(601, 19);
            btnSearch.Size = new Size(147, 31);
            btnSearch.Text = "btnSearch";
            btnSearch.Click += btnSearch_Click;

            btnSelect.Location = new Point(601, 58);
            btnSelect.Size = new Size(147, 31);
            btnSelect.Text = "btnSelect";
            btnSelect.Click += btnSelect_Click;

            btnSelectName.Location = new Point(601, 97);
            btnSelectName.Size = new Size(147, 31);
            btnSelectName.Text = "btnSelectName";
            btnSelectName.Click += btnSelectName_Click;

            panMap.BorderStyle = BorderStyle.Fixed3D;
            panMap.Location = new Point(3, 135);
            panMap.Size = new Size(768, 283);

            btnShowOnMap.Location = new Point(650, 29);
            btnShowOnMap.Size = new Size(98, 26);
            btnShowOnMap.Text = "btnShowOnMap";
            btnShowOnMap.Click += btnShowOnMap_Click;

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(777, 486);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Location = new Point(4, 26);
            pageMultimedia.Size = new Size(777, 486);
            pageMultimedia.Text = "pageMultimedia";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(784, 578);
            Controls.Add(tabsData);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "LocationEditDlg";
            tabsData.ResumeLayout();
            pageCommon.ResumeLayout();
            grpSearch.ResumeLayout();
            ResumeLayout();
        }
    }
}
