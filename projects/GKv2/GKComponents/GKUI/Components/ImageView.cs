﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GKCore;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.Types;

namespace GKUI.Components
{
    public class ImageView : UserControl, ILocalizable, IImageView
    {
        private ImageBox imageBox;
        private ToolStrip toolStrip;
        private ToolStripComboBox cbZoomLevels;
        private ToolStripButton btnSizeToFit;
        private ToolStripButton btnZoomIn;
        private ToolStripButton btnZoomOut;


        public List<NamedRegion> NamedRegions
        {
            get { return imageBox.NamedRegions; }
        }

        public bool ShowNamedRegionTips
        {
            get { return imageBox.ShowNamedRegionTips; }
            set { imageBox.ShowNamedRegionTips = value; }
        }

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

        public ExtRect SelectionRegion
        {
            get {
                RectangleF selectRegion = imageBox.SelectionRegion;
                return ExtRect.Create((int)selectRegion.Left, (int)selectRegion.Top, (int)selectRegion.Right, (int)selectRegion.Bottom);
            }
            set {
                imageBox.SelectionRegion = new RectangleF(value.Left, value.Top, value.GetWidth(), value.GetHeight());
            }
        }


        public ImageView()
        {
            InitializeComponent();

            FillZoomLevels();
        }

        public void Activate()
        {
            Select();
        }

        public void SetLocale()
        {
            btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
            btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
            btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);
        }

        #region Component design

        private void InitializeComponent()
        {
            SuspendLayout();

            btnSizeToFit = new ToolStripButton();
            btnSizeToFit.DisplayStyle = ToolStripItemDisplayStyle.Image;
            btnSizeToFit.Image = UIHelper.LoadResourceImage("Resources.btn_size_to_fit.png");
            btnSizeToFit.ImageTransparentColor = Color.Magenta;
            btnSizeToFit.Name = "btnSizeToFit";
            btnSizeToFit.Click += btnSizeToFit_Click;

            btnZoomIn = new ToolStripButton();
            btnZoomIn.DisplayStyle = ToolStripItemDisplayStyle.Image;
            btnZoomIn.Image = UIHelper.LoadResourceImage("Resources.btn_zoom_in.png");
            btnZoomIn.ImageTransparentColor = Color.Magenta;
            btnZoomIn.Name = "btnZoomIn";
            btnZoomIn.Click += btnZoomIn_Click;

            btnZoomOut = new ToolStripButton();
            btnZoomOut.DisplayStyle = ToolStripItemDisplayStyle.Image;
            btnZoomOut.Image = UIHelper.LoadResourceImage("Resources.btn_zoom_out.png");
            btnZoomOut.ImageTransparentColor = Color.Magenta;
            btnZoomOut.Name = "btnZoomOut";
            btnZoomOut.Click += btnZoomOut_Click;

            cbZoomLevels = new ToolStripComboBox();
            cbZoomLevels.DropDownStyle = ComboBoxStyle.DropDownList;
            cbZoomLevels.Name = "zoomLevelsToolStripComboBox";
            cbZoomLevels.Size = new Size(140, 28);
            cbZoomLevels.SelectedIndexChanged += cbZoomLevels_SelectedIndexChanged;

            toolStrip = new ToolStrip();
            toolStrip.Items.AddRange(new ToolStripItem[] {
                                         btnSizeToFit,
                                         btnZoomIn,
                                         btnZoomOut,
                                         new ToolStripSeparator(),
                                         cbZoomLevels,
                                         new ToolStripSeparator()});

            imageBox = new ImageBox();
            imageBox.AllowZoom = true;
            imageBox.BackColor = SystemColors.ControlDark;
            imageBox.ImageBorderColor = Color.AliceBlue;
            imageBox.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            imageBox.SelectionMode = ImageBoxSelectionMode.Zoom;
            imageBox.ZoomChanged += imageBox_ZoomChanged;
            imageBox.Dock = DockStyle.Fill;

            Controls.Add(imageBox);
            Controls.Add(toolStrip);
            Size = new Size(944, 389);
            ResumeLayout(true);
        }

        #endregion

        public void AddNamedRegion(string name, ExtRect region)
        {
            imageBox.NamedRegions.Add(new NamedRegion(name, region));
        }

        public void OpenImage(IImage image)
        {
            if (image != null) {
                OpenImage(((ImageHandler)image).Handle);
            }
        }

        public void OpenImage(Image image)
        {
            if (image != null) {
                imageBox.BeginUpdate();

                imageBox.Image = image;
                ZoomToFit();

                imageBox.EndUpdate();
            }
        }

        public void ZoomToFit()
        {
            imageBox.ZoomToFit();
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

        private void btnSizeToFit_Click(object sender, EventArgs e)
        {
            ZoomToFit();
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

        private void cbZoomLevels_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (cbZoomLevels.Focused) {
                // number w/out '%'
                int zoom = Convert.ToInt32(cbZoomLevels.Text.Substring(0, cbZoomLevels.Text.Length - 1));
                imageBox.Zoom = zoom;
            }
        }

        protected override void Select(bool directed, bool forward)
        {
            base.Select(directed, forward);
            imageBox.Select();
        }
    }
}
