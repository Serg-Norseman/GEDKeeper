/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Platform.Handlers;

namespace GKUI.Components
{
    public class ImageView : Panel, ILocalizable, IImageView
    {
        private GDMMultimediaRecord fMediaRecord;

        private ImageBox imageBox;
        private Panel toolStrip;
        private ComboBox cbZoomLevels;
        private Button btnSizeToFit;
        private Button btnZoomIn;
        private Button btnZoomOut;
        private Button btnPortrait;


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
            Focus();
        }

        public void SetLocale()
        {
            btnSizeToFit.ToolTip = LangMan.LS(LSID.SizeToFit);
            btnZoomIn.ToolTip = LangMan.LS(LSID.ZoomIn);
            btnZoomOut.ToolTip = LangMan.LS(LSID.ZoomOut);
        }

        #region Component design

        private void InitializeComponent()
        {
            SuspendLayout();

            btnSizeToFit = new Button();
            btnSizeToFit.Size = new Size(28, 28);
            btnSizeToFit.Image = UIHelper.LoadResourceImage("Resources.btn_size_to_fit.png");
            btnSizeToFit.Click += btnSizeToFit_Click;

            btnZoomIn = new Button();
            btnZoomIn.Size = new Size(28, 28);
            btnZoomIn.Image = UIHelper.LoadResourceImage("Resources.btn_zoom_in.png");
            btnZoomIn.Click += btnZoomIn_Click;

            btnZoomOut = new Button();
            btnZoomOut.Size = new Size(28, 28);
            btnZoomOut.Image = UIHelper.LoadResourceImage("Resources.btn_zoom_out.png");
            btnZoomOut.Click += btnZoomOut_Click;

            btnPortrait = new Button();
            btnPortrait.Size = new Size(28, 28);
            btnPortrait.Image = UIHelper.LoadResourceImage("Resources.btn_portrait.png");
            btnPortrait.Click += btnPortrait_Click;
            btnPortrait.Visible = false;

            cbZoomLevels = new ComboBox();
            cbZoomLevels.ReadOnly = true;
            cbZoomLevels.Size = new Size(140, 28);
            cbZoomLevels.TextChanged += cbZoomLevels_SelectedIndexChanged;

            toolStrip = new Panel();
            toolStrip.Content = new StackLayout() {
                Orientation = Orientation.Horizontal,
                Spacing = 10,
                Items = { btnSizeToFit, btnZoomIn, btnZoomOut, cbZoomLevels, btnPortrait }
            };

            imageBox = new ImageBox();
            imageBox.SelectionMode = ImageBoxSelectionMode.Rectangle;
            imageBox.ZoomChanged += imageBox_ZoomChanged;
            imageBox.SelectionRegionChanged += imageBox_SelectionRegionChanged;

            Content = new TableLayout() {
                Rows = {
                    new TableRow() {
                        Cells = { toolStrip }
                    },
                    new TableRow() {
                        ScaleHeight = true,
                        Cells = { imageBox }
                    }
                }
            };

            ResumeLayout();
        }

        #endregion

        public void AddNamedRegion(string name, ExtRect region)
        {
            imageBox.NamedRegions.Add(new NamedRegion(name, region));
        }

        public void OpenImage(GDMMultimediaRecord mediaRecord, IImage image)
        {
            if (image != null) {
                fMediaRecord = mediaRecord;
                OpenImage(((ImageHandler)image).Handle);
            }
        }

        private void OpenImage(Image image)
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
            cbZoomLevels.TextChanged -= cbZoomLevels_SelectedIndexChanged;

            cbZoomLevels.Items.Clear();
            foreach (int zoom in imageBox.ZoomLevels)
                cbZoomLevels.Items.Add(string.Format("{0}%", zoom));

            cbZoomLevels.TextChanged += cbZoomLevels_SelectedIndexChanged;
        }

        private void UpdateZoomLevels()
        {
            cbZoomLevels.TextChanged -= cbZoomLevels_SelectedIndexChanged;
            cbZoomLevels.Text = string.Format("{0}%", imageBox.Zoom);
            cbZoomLevels.TextChanged += cbZoomLevels_SelectedIndexChanged;
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

        private async void btnPortrait_Click(object sender, EventArgs e)
        {
            var mediaWin = ParentWindow as IMediaViewerWin;
            if (mediaWin == null) return;

            var baseWin = mediaWin.OwnerWindow as IBaseWindow;
            if (baseWin == null) return;

            if (await BaseController.SelectPhotoRegion(mediaWin, baseWin, fMediaRecord, UIHelper.Rt2Rt(imageBox.SelectionRegion))) {
                imageBox.SelectionRegion = RectangleF.Empty;
            }
        }

        private void imageBox_SelectionRegionChanged(object sender, EventArgs e)
        {
            btnPortrait.Visible = (ParentWindow is IMediaViewerWin && !imageBox.SelectionRegion.IsEmpty);
        }

        private void imageBox_ZoomChanged(object sender, EventArgs e)
        {
            UpdateZoomLevels();
        }

        private void cbZoomLevels_SelectedIndexChanged(object sender, EventArgs e)
        {
            /*if (cbZoomLevels.HasFocus)*/ {
                // number w/out '%'
                int zoom = Convert.ToInt32(cbZoomLevels.Text.Substring(0, cbZoomLevels.Text.Length - 1));
                imageBox.Zoom = zoom;
            }
        }
    }
}
