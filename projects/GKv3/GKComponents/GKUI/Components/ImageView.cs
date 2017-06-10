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
        private ImageBoxStub imageBox;
        private ToolBar toolStrip;
        //private ToolStripComboBox cbZoomLevels;
        public ButtonToolItem btnSizeToFit;
        public ButtonToolItem btnZoomIn;
        public ButtonToolItem btnZoomOut;


        /*public bool ShowToolbar
        {
            get { return toolStrip.Visible; }
            set { toolStrip.Visible = value; }
        }*/

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
            /*imageBox.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            imageBox.ImageBorderColor = Color.AliceBlue;
            imageBox.SelectionMode = ImageBoxSelectionMode.Zoom;
            imageBox.AllowDoubleClick = false;
            imageBox.AllowZoom = true;
            imageBox.InterpolationMode = InterpolationMode.HighQualityBicubic;*/
        }

        #region Component design

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            toolStrip = new ToolBar();
            btnSizeToFit = new ButtonToolItem();
            btnZoomIn = new ButtonToolItem();
            btnZoomOut = new ButtonToolItem();
            //cbZoomLevels = new ToolStripComboBox();
            imageBox = new ImageBoxStub();
            SuspendLayout();

            toolStrip.Items.AddRange(new ToolItem[] {
                                         btnSizeToFit,
                                         btnZoomIn,
                                         btnZoomOut,
                                         new SeparatorToolItem(),
                                         //cbZoomLevels,
                                         new SeparatorToolItem()});

            btnSizeToFit.Image = Bitmap.FromResource("Resources.btn_size_to_fit.gif");
            btnSizeToFit.Click += btnSizeToFit_Click;

            btnZoomIn.Image = Bitmap.FromResource("Resources.btn_zoom_in.gif");
            btnZoomIn.Click += btnZoomIn_Click;

            btnZoomOut.Image = Bitmap.FromResource("Resources.btn_zoom_out.gif");
            btnZoomOut.Click += btnZoomOut_Click;

            /*cbZoomLevels.DropDownStyle = ComboBoxStyle.DropDownList;
            cbZoomLevels.Name = "zoomLevelsToolStripComboBox";
            cbZoomLevels.Size = new Size(140, 28);
            cbZoomLevels.SelectedIndexChanged += new System.EventHandler(zoomLevelsToolStripComboBox_SelectedIndexChanged);*/

            imageBox.BackgroundColor = SystemColors.ControlBackground; // ControlDark;
            //imageBox.Dock = DockStyle.Fill;
            //imageBox.ZoomChanged += new System.EventHandler(imageBox_ZoomChanged);

            Content = imageBox;
            //Controls.Add(toolStrip);
            Size = new Size(944, 389);
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
            /*cbZoomLevels.Items.Clear();

            foreach (int zoom in imageBox.ZoomLevels)
                cbZoomLevels.Items.Add(string.Format("{0}%", zoom));*/
        }

        private void UpdateZoomLevels()
        {
            //cbZoomLevels.Text = string.Format("{0}%", imageBox.Zoom);
        }

        /*private void zoomLevelsToolStripComboBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            int zoom = Convert.ToInt32(cbZoomLevels.Text.Substring(0, cbZoomLevels.Text.Length - 1));
            imageBox.Zoom = zoom;
        }*/

        /*protected override void Select(bool directed, bool forward)
        {
            base.Select(directed, forward);
            imageBox.Select();
        }*/
    }
}
