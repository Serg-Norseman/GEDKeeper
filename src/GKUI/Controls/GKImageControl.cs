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
            //this.OpenImage(Properties.Resources.Sample);
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

        private void actualSizeToolStripButton_Click(object sender, EventArgs e)
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

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            using (FileDialog dialog = new OpenFileDialog())
            {
                dialog.Filter = "All Supported Images (*.bmp;*.dib;*.rle;*.gif;*.jpg;*.png)|*.bmp;*.dib;*.rle;*.gif;*.jpg;*.png|Bitmaps (*.bmp;*.dib;*.rle)|*.bmp;*.dib;*.rle|Graphics Interchange Format (*.gif)|*.gif|Joint Photographic Experts (*.jpg)|*.jpg|Portable Network Graphics (*.png)|*.png|All Files (*.*)|*.*";
                dialog.DefaultExt = "png";

                if (dialog.ShowDialog(this) == DialogResult.OK)
                {
                    try
                    {
                        this.OpenImage(Image.FromFile(dialog.FileName));
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(ex.Message, this.Text, MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                }
            }
        }

        private void zoomInToolStripButton_Click(object sender, EventArgs e)
        {
            imageBox.ZoomIn();
        }

        private void zoomLevelsToolStripComboBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            int zoom;
            zoom = Convert.ToInt32(zoomLevelsToolStripComboBox.Text.Substring(0, zoomLevelsToolStripComboBox.Text.Length - 1));
            imageBox.Zoom = zoom;
        }

        private void zoomOutToolStripButton_Click(object sender, EventArgs e)
        {
            imageBox.ZoomOut();
        }

    }
}
