/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    public class ImageView : Panel
    {
        private ImageBox imageBox;
        private Panel toolStrip;
        private ComboBox cbZoomLevels;

        public Button btnSizeToFit;
        public Button btnZoomIn;
        public Button btnZoomOut;


        public bool ShowToolbar
        {
            get { return toolStrip.Visible; }
            set { toolStrip.Visible = value; }
        }

        public ImageBoxSelectionMode SelectionMode
        {
            get { return imageBox.SelectionMode; }
            set { imageBox.SelectionMode = value; }
        }

        public RectangleF SelectionRegion
        {
            get { return imageBox.SelectionRegion; }
            set { imageBox.SelectionRegion = value; }
        }


        public ImageView()
        {
            InitializeComponent();

            FillZoomLevels();
        }

        #region Component design

        private void InitializeComponent()
        {
            SuspendLayout();

            btnSizeToFit = new Button();
            btnSizeToFit.Image = Bitmap.FromResource("Resources.btn_size_to_fit.png");
            btnSizeToFit.Click += btnSizeToFit_Click;

            btnZoomIn = new Button();
            btnZoomIn.Image = Bitmap.FromResource("Resources.btn_zoom_in.png");
            btnZoomIn.Click += btnZoomIn_Click;

            btnZoomOut = new Button();
            btnZoomOut.Image = Bitmap.FromResource("Resources.btn_zoom_out.png");
            btnZoomOut.Click += btnZoomOut_Click;

            cbZoomLevels = new ComboBox();
            cbZoomLevels.ReadOnly = true;
            cbZoomLevels.Size = new Size(140, 28);
            cbZoomLevels.SelectedIndexChanged += zoomLevelsToolStripComboBox_SelectedIndexChanged;

            toolStrip = new Panel();
            toolStrip.Content = new StackLayout() {
                Orientation = Orientation.Horizontal,
                Spacing = 10,
                Items = {
                    btnSizeToFit,
                    btnZoomIn,
                    btnZoomOut,
                    cbZoomLevels
                }
            };

            imageBox = new ImageBox();
            imageBox.AllowDoubleClick = false;
            imageBox.AllowZoom = true;
            imageBox.BackgroundColor = SystemColors.ControlBackground; // ControlDark;
            imageBox.ImageBorderColor = Colors.AliceBlue;
            imageBox.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            imageBox.InterpolationMode = ImageInterpolation.High;
            imageBox.SelectionMode = ImageBoxSelectionMode.Zoom;
            imageBox.ZoomChanged += imageBox_ZoomChanged;

            Content = new StackLayout() {
                Orientation = Orientation.Vertical,
                Items = {
                    toolStrip,
                    imageBox
                }
            };

            ResumeLayout();
        }

        #endregion

        public void OpenImage(Image image)
        {
            if (image != null) {
                imageBox.BeginUpdate();
                imageBox.Image = image;
                imageBox.ZoomToFit();
                imageBox.EndUpdate();

                UpdateZoomLevels();
            }
        }

        private void btnSizeToFit_Click(object sender, EventArgs e)
        {
            imageBox.ZoomToFit();
            UpdateZoomLevels();
        }

        private void btnZoomIn_Click(object sender, EventArgs e)
        {
            imageBox.ZoomIn();
        }

        private void btnZoomOut_Click(object sender, EventArgs e)
        {
            imageBox.ZoomOut();
        }

        private void imageBox_ZoomChanged(object sender, EventArgs e)
        {
            UpdateZoomLevels();
        }

        private void FillZoomLevels()
        {
            cbZoomLevels.Items.Clear();

            foreach (int zoom in imageBox.ZoomLevels)
                cbZoomLevels.Items.Add(string.Format("{0}%", zoom));
        }

        private void UpdateZoomLevels()
        {
            cbZoomLevels.Text = string.Format("{0}%", imageBox.Zoom);
        }

        private void zoomLevelsToolStripComboBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            int zoom = Convert.ToInt32(cbZoomLevels.Text.Substring(0, cbZoomLevels.Text.Length - 1));
            imageBox.Zoom = zoom;
        }
    }
}
