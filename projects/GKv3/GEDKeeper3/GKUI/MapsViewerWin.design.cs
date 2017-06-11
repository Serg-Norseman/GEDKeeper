using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI
{
    partial class MapsViewerWin
    {
        //private StatusBar StatusBar1; // FIXME: GKv3 DevRestriction
        private TabControl PageControl1;
        private TabPage pagePlaces;
        private TreeView tvPlaces;
        private Panel Panel1;
        private GroupBox grpSelection;
        private ComboBox cmbPersons;
        private CheckBox chkResidence;
        private CheckBox chkDeath;
        private CheckBox chkBirth;
        private Button btnSelectPlaces;
        private Button btnSaveImage;
        private RadioButton radTotal;
        private RadioButton radSelected;
        private CheckBox chkLinesVisible;

        private void InitializeComponent()
        {
            //StatusBar1 = new StatusBar();
            PageControl1 = new TabControl();
            pagePlaces = new TabPage();
            tvPlaces = new TreeView();
            grpSelection = new GroupBox();
            cmbPersons = new ComboBox();
            chkResidence = new CheckBox();
            chkDeath = new CheckBox();
            chkBirth = new CheckBox();
            btnSelectPlaces = new Button();
            btnSaveImage = new Button();
            radTotal = new RadioButton();
            radSelected = new RadioButton();
            chkLinesVisible = new CheckBox();
            Panel1 = new Panel();

            SuspendLayout();

            //StatusBar1.Location = new Point(0, 549);
            //StatusBar1.Size = new Size(1101, 23);

            PageControl1.Pages.Add(pagePlaces);
            PageControl1.SelectedIndex = 0;
            PageControl1.Size = new Size(405, 549);

            pagePlaces.Size = new Size(397, 519);
            pagePlaces.Text = "pagePlaces";
            pagePlaces.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
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

            tvPlaces.Size = new Size(397, 294);
            tvPlaces.MouseDoubleClick += TreePlaces_DoubleClick;

            grpSelection.Size = new Size(397, 225);
            grpSelection.Text = "grpSelection";
            grpSelection.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
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
                        Cells = { btnSelectPlaces, btnSaveImage }
                    }
                }
            };

            cmbPersons.ReadOnly = true;
            cmbPersons.Size = new Size(371, 25);

            chkResidence.Size = new Size(152, 21);
            chkResidence.Text = "chkResidence";

            chkDeath.Size = new Size(120, 21);
            chkDeath.Text = "chkDeath";

            chkBirth.Size = new Size(137, 21);
            chkBirth.Text = "chkBirth";

            btnSelectPlaces.Enabled = false;
            btnSelectPlaces.Size = new Size(105, 30);
            btnSelectPlaces.Text = "btnSelectPlaces";
            btnSelectPlaces.Click += btnSelectPlaces_Click;

            btnSaveImage.Size = new Size(170, 30);
            btnSaveImage.Text = "btnSaveImage";
            btnSaveImage.Click += btnSaveImage_Click;

            radTotal.Size = new Size(127, 21);
            radTotal.Text = "radTotal";
            radTotal.Click += radTotal_Click;

            radSelected.Size = new Size(180, 21);
            radSelected.Text = "radSelected";
            radSelected.Click += radTotal_Click;

            chkLinesVisible.Checked = true;
            chkLinesVisible.Size = new Size(155, 21);
            chkLinesVisible.Text = "chkLinesVisible";

            Panel1.Size = new Size(696, 549);

            //Controls.Add(StatusBar1);
            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { PageControl1, Panel1 }
                    }
                }
            };

            ClientSize = new Size(1101, 572);
            Title = "MapsViewerWin";
            KeyDown += MapsViewerWin_KeyDown;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
