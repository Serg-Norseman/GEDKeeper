using System;
using System.Drawing;
using System.Windows.Forms;

using Cyotek.Windows.Forms;

namespace GKUI.Controls
{
    public partial class GKImageControl : UserControl
    {
        public GKImageControl()
        {
            InitializeComponent();

            this.FillZoomLevels();
            imageBox.SelectionMode = ImageBoxSelectionMode.Zoom;
        }

        private void FillZoomLevels()
        {
            zoomLevelsToolStripComboBox.Items.Clear();

            foreach (int zoom in imageBox.ZoomLevels)
                zoomLevelsToolStripComboBox.Items.Add(string.Format("{0}%", zoom));
        }

        public void OpenImage(Image image)
        {
            imageBox.Image = image;
            imageBox.ZoomToFit();

            this.UpdateStatusBar();
        }

        private void UpdateStatusBar()
        {
            zoomLevelsToolStripComboBox.Text = string.Format("{0}%", imageBox.Zoom);
        }

        private void btnSizeToFit_Click(object sender, EventArgs e)
        {
            imageBox.ZoomToFit();
            this.UpdateStatusBar();
        }

        private void imageBox_ZoomChanged(object sender, EventArgs e)
        {
            this.UpdateStatusBar();
        }

        private void imageBox_ZoomLevelsChanged(object sender, EventArgs e)
        {
            this.FillZoomLevels();
        }

        private void btnZoomIn_Click(object sender, EventArgs e)
        {
            imageBox.ZoomIn();
        }

        private void zoomLevelsToolStripComboBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            int zoom = Convert.ToInt32(zoomLevelsToolStripComboBox.Text.Substring(0, zoomLevelsToolStripComboBox.Text.Length - 1));
            imageBox.Zoom = zoom;
        }

        private void btnZoomOut_Click(object sender, EventArgs e)
        {
            imageBox.ZoomOut();
            //this.btnSizeToFit.Image = global::GKImageControl.iSizeToFit2;
            //this.btnZoomIn.Image = global::GKResources.iZoomIn2;
            //this.btnZoomOut.Image = global::GKResources.iZoomOut2;
        }
    }
}
