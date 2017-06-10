using System;
using Eto.Drawing;
using Eto.Forms;

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

        protected override void Dispose(bool Disposing)
        {
            if (Disposing)
            {
                fPlaces.Dispose();
                fMapPoints.Dispose();
            }
            base.Dispose(Disposing);
        }

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
            PageControl1.SuspendLayout();
            pagePlaces.SuspendLayout();
            grpSelection.SuspendLayout();
            SuspendLayout();

            //StatusBar1.Location = new Point(0, 549);
            //StatusBar1.Size = new Size(1101, 23);

            PageControl1.Controls.Add(pagePlaces);
            PageControl1.Dock = DockStyle.Left;
            PageControl1.Location = new Point(0, 0);
            PageControl1.SelectedIndex = 0;
            PageControl1.Size = new Size(405, 549);

            pagePlaces.Controls.Add(tvPlaces);
            pagePlaces.Controls.Add(grpSelection);
            pagePlaces.Location = new Point(4, 26);
            pagePlaces.Size = new Size(397, 519);
            pagePlaces.Text = "pagePlaces";

            tvPlaces.Dock = DockStyle.Fill;
            tvPlaces.Location = new Point(0, 225);
            tvPlaces.Size = new Size(397, 294);
            tvPlaces.DoubleClick += TreePlaces_DoubleClick;

            grpSelection.Controls.Add(cmbPersons);
            grpSelection.Controls.Add(chkResidence);
            grpSelection.Controls.Add(chkDeath);
            grpSelection.Controls.Add(chkBirth);
            grpSelection.Controls.Add(btnSelectPlaces);
            grpSelection.Controls.Add(btnSaveImage);
            grpSelection.Controls.Add(radTotal);
            grpSelection.Controls.Add(radSelected);
            grpSelection.Controls.Add(chkLinesVisible);
            grpSelection.Dock = DockStyle.Top;
            grpSelection.Location = new Point(0, 0);
            grpSelection.Size = new Size(397, 225);
            grpSelection.Text = "grpSelection";

            cmbPersons.ReadOnly = true;
            cmbPersons.Location = new Point(11, 126);
            cmbPersons.Size = new Size(371, 25);

            chkResidence.Location = new Point(27, 78);
            chkResidence.Size = new Size(152, 21);
            chkResidence.Text = "chkResidence";

            chkDeath.Location = new Point(27, 58);
            chkDeath.Size = new Size(120, 21);
            chkDeath.Text = "chkDeath";

            chkBirth.Location = new Point(27, 39);
            chkBirth.Size = new Size(137, 21);
            chkBirth.Text = "chkBirth";

            btnSelectPlaces.Enabled = false;
            btnSelectPlaces.Location = new Point(277, 185);
            btnSelectPlaces.Size = new Size(105, 30);
            btnSelectPlaces.Text = "btnSelectPlaces";
            btnSelectPlaces.Click += btnSelectPlaces_Click;

            btnSaveImage.Location = new Point(11, 185);
            btnSaveImage.Size = new Size(170, 30);
            btnSaveImage.Text = "btnSaveImage";
            btnSaveImage.Click += btnSaveImage_Click;

            radTotal.Location = new Point(11, 19);
            radTotal.Size = new Size(127, 21);
            radTotal.Text = "radTotal";
            radTotal.Click += radTotal_Click;

            radSelected.Location = new Point(11, 106);
            radSelected.Size = new Size(180, 21);
            radSelected.Text = "radSelected";
            radSelected.Click += radTotal_Click;

            chkLinesVisible.Checked = true;
            chkLinesVisible.Location = new Point(11, 155);
            chkLinesVisible.Size = new Size(155, 21);
            chkLinesVisible.Text = "chkLinesVisible";

            Panel1.Dock = DockStyle.Fill;
            Panel1.Location = new Point(405, 0);
            Panel1.Size = new Size(696, 549);

            ClientSize = new Size(1101, 572);
            Controls.Add(Panel1);
            Controls.Add(PageControl1);
            //Controls.Add(StatusBar1);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Title = "MapsViewerWin";
            KeyDown += MapsViewerWin_KeyDown;
            PageControl1.ResumeLayout();
            pagePlaces.ResumeLayout();
            grpSelection.ResumeLayout();
            ResumeLayout();
        }
    }
}
