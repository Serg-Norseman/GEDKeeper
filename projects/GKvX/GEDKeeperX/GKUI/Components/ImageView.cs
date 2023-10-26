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
using GKCore;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using Xamarin.Forms;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Platform.Handlers;

namespace GKUI.Components
{
    public class ImageView : ContentView, IImageView
    {
        private ImageBox imageBox;
        private StackLayout toolStrip;
        private Picker cbZoomLevels;
        private Button btnSizeToFit;
        private Button btnZoomIn;
        private Button btnZoomOut;


        public List<NamedRegion> NamedRegions
        {
            get { return/* imageBox.NamedRegions*/ null; }
        }

        public bool ShowNamedRegionTips
        {
            get { return /*imageBox.ShowNamedRegionTips*/ false; }
            set { /*imageBox.ShowNamedRegionTips = value;*/ }
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
                return ExtRect.Empty;
                //RectangleF selectRegion = imageBox.SelectionRegion;
                //return ExtRect.Create((int)selectRegion.Left, (int)selectRegion.Top, (int)selectRegion.Right, (int)selectRegion.Bottom);
            }
            set {
                //imageBox.SelectionRegion = new RectangleF(value.Left, value.Top, value.GetWidth(), value.GetHeight());
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
            //btnSizeToFit.ToolTip = LangMan.LS(LSID.SizeToFit);
            //btnZoomIn.ToolTip = LangMan.LS(LSID.ZoomIn);
            //btnZoomOut.ToolTip = LangMan.LS(LSID.ZoomOut);
        }

        #region Component design

        private void InitializeComponent()
        {
            btnSizeToFit = new Button();
            //btnSizeToFit.Size = new Size(28, 28);
            btnSizeToFit.ImageSource = UIHelper.LoadResourceImage("Resources.btn_size_to_fit.png");
            btnSizeToFit.Clicked += btnSizeToFit_Click;

            btnZoomIn = new Button();
            //btnZoomIn.Size = new Size(28, 28);
            btnZoomIn.ImageSource = UIHelper.LoadResourceImage("Resources.btn_zoom_in.png");
            btnZoomIn.Clicked += btnZoomIn_Click;

            btnZoomOut = new Button();
            //btnZoomOut.Size = new Size(28, 28);
            btnZoomOut.ImageSource = UIHelper.LoadResourceImage("Resources.btn_zoom_out.png");
            btnZoomOut.Clicked += btnZoomOut_Click;

            cbZoomLevels = new Picker();
            //cbZoomLevels.ReadOnly = true;
            //cbZoomLevels.Size = new Size(140, 28);
            //cbZoomLevels.TextChanged += cbZoomLevels_SelectedIndexChanged;

            toolStrip = new StackLayout() {
                Orientation = StackOrientation.Horizontal,
                Spacing = 10,
                Children = {
                    btnSizeToFit,
                    btnZoomIn,
                    btnZoomOut,
                    cbZoomLevels
                },
                VerticalOptions = LayoutOptions.Start
            };

            imageBox = new ImageBox();
            //imageBox.AllowZoom = true;
            imageBox.BackgroundColor = Color.Gray;
            imageBox.ImageBorderColor = Color.AliceBlue;
            imageBox.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            imageBox.SelectionMode = ImageBoxSelectionMode.Zoom;
            //imageBox.ZoomChanged += imageBox_ZoomChanged;
            imageBox.VerticalOptions = LayoutOptions.EndAndExpand;

            Content = new StackLayout() {
                Orientation = StackOrientation.Vertical,
                Children = {
                    toolStrip,
                    imageBox
                }
            };
        }

        #endregion

        public void AddNamedRegion(string name, ExtRect region)
        {
            //imageBox.NamedRegions.Add(new NamedRegion(name, region));
        }

        public void OpenImage(IImage image)
        {
            if (image != null) {
                OpenImage(((XFImageHandler)image).Handle);
            }
        }

        public void OpenImage(ImageSource image)
        {
            if (image != null) {
                //imageBox.BeginUpdate();

                imageBox.Image = image;
                ZoomToFit();

                //imageBox.EndUpdate();
            }
        }

        public void ZoomToFit()
        {
            imageBox.ZoomToFit();
            UpdateZoomLevels();
        }

        private void FillZoomLevels()
        {
            //cbZoomLevels.TextChanged -= cbZoomLevels_SelectedIndexChanged;

            cbZoomLevels.Items.Clear();
            //foreach (int zoom in imageBox.ZoomLevels)
            //cbZoomLevels.Items.Add(string.Format("{0}%", zoom));

            //cbZoomLevels.TextChanged += cbZoomLevels_SelectedIndexChanged;
        }

        private void UpdateZoomLevels()
        {
            //cbZoomLevels.TextChanged -= cbZoomLevels_SelectedIndexChanged;
            //cbZoomLevels.Text = string.Format("{0}%", imageBox.Zoom);
            //cbZoomLevels.TextChanged += cbZoomLevels_SelectedIndexChanged;
        }

        private void btnSizeToFit_Click(object sender, EventArgs e)
        {
            ZoomToFit();
        }

        private void btnZoomIn_Click(object sender, EventArgs e)
        {
            //imageBox.ZoomIn();
        }

        private void btnZoomOut_Click(object sender, EventArgs e)
        {
            //imageBox.ZoomOut();
        }

        private void imageBox_ZoomChanged(object sender, EventArgs e)
        {
            UpdateZoomLevels();
        }

        private void cbZoomLevels_SelectedIndexChanged(object sender, EventArgs e)
        {
            /*if (cbZoomLevels.HasFocus)*/
            {
                // number w/out '%'
                //int zoom = Convert.ToInt32(cbZoomLevels.Text.Substring(0, cbZoomLevels.Text.Length - 1));
                //imageBox.Zoom = zoom;
            }
        }
    }
}
