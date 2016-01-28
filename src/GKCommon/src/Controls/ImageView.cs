using System;
using System.Drawing;
using System.Windows.Forms;

using Cyotek.Windows.Forms;

namespace GKCommon.Controls
{
    public partial class ImageView : UserControl
    {
    	public bool ShowToolbar
    	{
    		get {
    			return this.toolStrip.Visible;
    		}
    		set {
    			this.toolStrip.Visible = value;
    		}
    	}

        public ImageView()
        {
            InitializeComponent();

            this.FillZoomLevels();
            imageBox.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            imageBox.ImageBorderColor = Color.AliceBlue;
            imageBox.SelectionMode = ImageBoxSelectionMode.Zoom;
            imageBox.ShowPixelGrid = true;
        }

        public void OpenImage(Image image)
        {
            imageBox.Image = image;
            imageBox.ZoomToFit();

            this.UpdateZoomLevels();
        }

        private void FillZoomLevels()
        {
            zoomLevelsToolStripComboBox.Items.Clear();

            foreach (int zoom in imageBox.ZoomLevels)
                zoomLevelsToolStripComboBox.Items.Add(string.Format("{0}%", zoom));
        }

        private void UpdateZoomLevels()
        {
            zoomLevelsToolStripComboBox.Text = string.Format("{0}%", imageBox.Zoom);
        }

        private void btnSizeToFit_Click(object sender, EventArgs e)
        {
            imageBox.ZoomToFit();
            this.UpdateZoomLevels();
        }

        private void imageBox_ZoomChanged(object sender, EventArgs e)
        {
            this.UpdateZoomLevels();
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
        }
    }
}
