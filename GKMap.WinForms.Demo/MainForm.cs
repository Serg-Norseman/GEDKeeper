using System;
using System.Drawing;
using System.Globalization;
using System.Windows.Forms;
using GKMap.MapProviders;

namespace GKMap.WinForms.Demo
{
    public partial class MainForm : Form
    {
        public MainForm()
        {
            InitializeComponent();

            if (!GMapControl.IsDesignerHosted) {
                MapBrowser.MapControl.OnMapTypeChanged += MainMap_OnMapTypeChanged;
                MapBrowser.MapControl.OnMapZoomChanged += MainMap_OnMapZoomChanged;

                // get zoom  
                trkZoom.Minimum = MapBrowser.MapControl.MinZoom * 100;
                trkZoom.Maximum = MapBrowser.MapControl.MaxZoom * 100;
                trkZoom.TickFrequency = 100;

                // get map types
#if !MONO   // mono doesn't handle it, so we 'lost' provider list ;]
                cmbMapType.ValueMember = "Name";
                cmbMapType.DataSource = GMapProviders.List;
                cmbMapType.SelectedItem = MapBrowser.MapControl.MapProvider;
#endif

                if (MapBrowser.MapControl.Zoom >= MapBrowser.MapControl.MinZoom && MapBrowser.MapControl.Zoom <= MapBrowser.MapControl.MaxZoom) {
                    trkZoom.Value = (int)MapBrowser.MapControl.Zoom * 100;
                }
            }
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

        // change map type
        private void cmbMapType_DropDownClosed(object sender, EventArgs e)
        {
            MapBrowser.MapControl.MapProvider = cmbMapType.SelectedItem as GMapProvider;
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
                GeocoderStatusCode status = MapBrowser.MapControl.SetPositionByKeywords(textBoxGeo.Text);
                if (status != GeocoderStatusCode.Success) {
                    MessageBox.Show("Geocoder can't find: '" + textBoxGeo.Text + "', reason: " + status, @"GKMap", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                }
            }
        }

        private void MainMap_OnMapTypeChanged(GMapProvider type)
        {
            cmbMapType.SelectedItem = type;
            trkZoom.Minimum = MapBrowser.MapControl.MinZoom * 100;
            trkZoom.Maximum = MapBrowser.MapControl.MaxZoom * 100;
            MapBrowser.MapControl.ZoomAndCenterMarkers("objects");
        }

        private void MainMap_OnMapZoomChanged()
        {
            trkZoom.Value = (int)(MapBrowser.MapControl.Zoom * 100.0);
        }

        private void trkZoom_ValueChanged(object sender, EventArgs e)
        {
            MapBrowser.MapControl.Zoom = trkZoom.Value / 100.0;
        }

        private void btnZoomUp_Click(object sender, EventArgs e)
        {
            MapBrowser.MapControl.Zoom = ((int)MapBrowser.MapControl.Zoom) + 1;
        }

        private void btnZoomDown_Click(object sender, EventArgs e)
        {
            MapBrowser.MapControl.Zoom = ((int)(MapBrowser.MapControl.Zoom + 0.99)) - 1;
        }

        private void btnAddMarker_Click(object sender, EventArgs e)
        {
            MapBrowser.AddMarker(MapBrowser.TargetPosition);
        }

        // saves current map view
        private void btnSaveView_Click(object sender, EventArgs e)
        {
            try {
                using (SaveFileDialog sfd = new SaveFileDialog()) {
                    sfd.Filter = "PNG (*.png)|*.png";
                    sfd.FileName = "GKMap image";

                    Image tmpImage = MapBrowser.MapControl.ToImage();
                    if (tmpImage != null) {
                        using (tmpImage) {
                            if (sfd.ShowDialog() == DialogResult.OK) {
                                tmpImage.Save(sfd.FileName);

                                MessageBox.Show("Image saved: " + sfd.FileName, "GKMap", MessageBoxButtons.OK, MessageBoxIcon.Information);
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                MessageBox.Show("Image failed to save: " + ex.Message, "GKMap", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        // clear markers and all
        private void btnClearAll_Click(object sender, EventArgs e)
        {
            MapBrowser.Routes.Routes.Clear();
            MapBrowser.Polygons.Polygons.Clear();
            MapBrowser.Objects.Markers.Clear();
        }

        // zoom to max for markers
        private void btnZoomCenter_Click(object sender, EventArgs e)
        {
            MapBrowser.MapControl.ZoomAndCenterMarkers("objects");
        }
    }
}
