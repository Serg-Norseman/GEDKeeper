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
using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKCore.Types;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class ImageView : ContentView, ILocalizable, IImageView
    {
        private GDMMultimediaRecord fMediaRecord;
        private MediaViewerController fController;

        private ImageBox imageBox;
        private StackLayout toolStrip;
        private GKComboBox cbZoomLevels;
        private Button btnSizeToFit;
        private Button btnZoomIn;
        private Button btnZoomOut;


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
            get { return toolStrip.IsVisible; }
            set { toolStrip.IsVisible = value; }
        }

        public ImageBoxSelectionMode SelectionMode
        {
            get { return imageBox.SelectionMode; }
            set { imageBox.SelectionMode = value; }
        }

        public ExtRect SelectionRegion
        {
            get {
                var selectRegion = imageBox.SelectionRegion;
                return ExtRect.Create((int)selectRegion.Left, (int)selectRegion.Top, (int)selectRegion.Right, (int)selectRegion.Bottom);
            }
            set {
                imageBox.SelectionRegion = new Rectangle(value.Left, value.Top, value.GetWidth(), value.GetHeight());
            }
        }


        public bool Enabled { get; set; }


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
            // not used
        }

        #region Component design

        private void InitializeComponent()
        {
            btnSizeToFit = new Button();
            btnSizeToFit.ImageSource = UIHelper.LoadResourceImage("Resources.btn_size_to_fit.png");
            btnSizeToFit.Clicked += btnSizeToFit_Click;

            btnZoomIn = new Button();
            btnZoomIn.ImageSource = UIHelper.LoadResourceImage("Resources.btn_zoom_in.png");
            btnZoomIn.Clicked += btnZoomIn_Click;

            btnZoomOut = new Button();
            btnZoomOut.ImageSource = UIHelper.LoadResourceImage("Resources.btn_zoom_out.png");
            btnZoomOut.Clicked += btnZoomOut_Click;

            cbZoomLevels = new GKComboBox();
            cbZoomLevels.WidthRequest = 140;
            cbZoomLevels.SelectedIndexChanged += cbZoomLevels_SelectedIndexChanged;

            toolStrip = new StackLayout() {
                Orientation = StackOrientation.Horizontal,
                Spacing = 8,
                Children = { btnSizeToFit, btnZoomIn, btnZoomOut, cbZoomLevels },
                VerticalOptions = LayoutOptions.Start
            };

            imageBox = new ImageBox();
            imageBox.SelectionMode = ImageBoxSelectionMode.Zoom;
            imageBox.ZoomChanged += imageBox_ZoomChanged;
            imageBox.VerticalOptions = LayoutOptions.FillAndExpand;

            Content = new StackLayout() {
                Orientation = StackOrientation.Vertical,
                Children = { toolStrip, imageBox }
            };
        }

        #endregion

        public void AddNamedRegion(string name, ExtRect region)
        {
            imageBox.NamedRegions.Add(new NamedRegion(name, region));
        }

        public void OpenImage(MediaViewerController controller, IImage image)
        {
            fController = controller;
            if (fController != null) {
                fMediaRecord = fController.MultimediaRecord;
            }

            if (image != null) {
                OpenImage(((SKImageHandler)image).Handle);
            }
        }

        public void OpenImage(SKImageImageSource image)
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
            cbZoomLevels.SelectedIndexChanged -= cbZoomLevels_SelectedIndexChanged;

            cbZoomLevels.Items.Clear();
            foreach (int zoom in imageBox.ZoomLevels)
                cbZoomLevels.AddItem(string.Format("{0}%", zoom), zoom);

            cbZoomLevels.SelectedIndexChanged += cbZoomLevels_SelectedIndexChanged;
        }

        private void UpdateZoomLevels()
        {
            cbZoomLevels.SelectedIndexChanged -= cbZoomLevels_SelectedIndexChanged;
            //cbZoomLevels.Text = string.Format("{0}%", imageBox.Zoom);
            cbZoomLevels.SelectedIndexChanged += cbZoomLevels_SelectedIndexChanged;
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
            /*if (cbZoomLevels.HasFocus)*/ {
                // number w/out '%'
                //int zoom = Convert.ToInt32(cbZoomLevels.Text.Substring(0, cbZoomLevels.Text.Length - 1));
                //imageBox.Zoom = zoom;
            }
        }
    }
}
