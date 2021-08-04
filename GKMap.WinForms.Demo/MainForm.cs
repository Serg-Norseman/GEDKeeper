using System;
using System.Globalization;
using System.Windows.Forms;

namespace GKMap.WinForms.Demo
{
    public partial class MainForm : Form
    {
        public MainForm()
        {
            InitializeComponent();
        }

        private void MainForm_Load(object sender, EventArgs e)
        {
            Activate();
            TopMost = true;
            TopMost = false;

            // get position
            txtLat.Text = MapBrowser.MapControl.Position.Lat.ToString(CultureInfo.InvariantCulture);
            txtLng.Text = MapBrowser.MapControl.Position.Lng.ToString(CultureInfo.InvariantCulture);

            // add some points in lithuania
            MapBrowser.AddLocationMarker("Lithuania, " + "Kaunas");
            MapBrowser.AddLocationMarker("Lithuania, " + "Klaipėda");
            MapBrowser.AddLocationMarker("Lithuania, " + "Šiauliai");
            MapBrowser.AddLocationMarker("Lithuania, " + "Panevėžys");

            MapBrowser.MapControl.ZoomAndCenterMarkers(null);
        }

        private void btnGoto_Click(object sender, EventArgs e)
        {
            try {
                double lat = double.Parse(txtLat.Text, CultureInfo.InvariantCulture);
                double lng = double.Parse(txtLng.Text, CultureInfo.InvariantCulture);

                MapBrowser.MapControl.Position = new PointLatLng(lat, lng);
            } catch (Exception ex) {
                MessageBox.Show("incorrect coordinate format: " + ex.Message);
            }
        }

        private void textBoxGeo_KeyPress(object sender, KeyPressEventArgs e)
        {
            if ((Keys)e.KeyChar == Keys.Enter) {
                GeoCoderStatusCode status = MapBrowser.MapControl.SetPositionByKeywords(textBoxGeo.Text);
                if (status != GeoCoderStatusCode.G_GEO_SUCCESS) {
                    MessageBox.Show("Geocoder can't find: '" + textBoxGeo.Text + "', reason: " + status, @"GKMap", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                }
            }
        }

        private void btnAddMarker_Click(object sender, EventArgs e)
        {
            MapBrowser.AddMarker(MapBrowser.TargetPosition);
        }
    }
}
