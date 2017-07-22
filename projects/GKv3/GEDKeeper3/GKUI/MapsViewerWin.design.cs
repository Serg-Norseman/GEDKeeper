using Eto.Forms;
using GKUI.Components;

namespace GKUI
{
    partial class MapsViewerWin
    {
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
            SuspendLayout();

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

            btnSaveImage = new Button();
            btnSaveImage.Text = "btnSaveImage";
            btnSaveImage.Click += btnSaveImage_Click;

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
                        Cells = { TableLayout.Horizontal(10, btnSelectPlaces, btnSaveImage) }
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

            PageControl1 = new TabControl();
            PageControl1.Pages.Add(pagePlaces);

            Panel1 = new Panel();

            Content = TableLayout.Horizontal(4, PageControl1, Panel1);

            Title = "MapsViewerWin";
            KeyDown += MapsViewerWin_KeyDown;

            UIHelper.SetPredefProperties(this, 1100, 570);
            ResumeLayout();
        }
    }
}
