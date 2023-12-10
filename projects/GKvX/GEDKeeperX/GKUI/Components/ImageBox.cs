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
using GKCore.Types;
using SkiaSharp;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;
using Graphics = SkiaSharp.SKCanvas;

namespace GKUI.Components
{
    /// <summary>
    ///   Component for displaying images with support for scrolling and zooming.
    /// </summary>
    public sealed class ImageBox : ScrollablePanel
    {
        private readonly List<NamedRegion> fNamedRegions;
        private readonly List<int> fZoomLevels;

        private bool fAllowZoom;
        private bool fAutoPan;
        private int fDropShadowSize;
        private SKImageImageSource fImage;
        private Color fImageBorderColor;
        private int fScaledImageHeight;
        private int fScaledImageWidth;
        private Color fSelectionColor;
        private ImageBoxSelectionMode fSelectionMode;
        private Rectangle fSelectionRegion;
        private bool fShowNamedRegionTips;
        private bool fSizeToFit;
        private int fUpdateCount;
        private int fZoom;
        private Size fImageSize;
        private float fZoomFactor;
        private Thickness fImagePadding;


        /// <summary>
        ///   Occurs when the Image property is changed.
        /// </summary>
        public event EventHandler ImageChanged;

        /// <summary>
        ///   Occurs when the Zoom property is changed.
        /// </summary>
        public event EventHandler ZoomChanged;


        /// <summary>
        ///   Gets or sets a value indicating whether the user can change the zoom level.
        /// </summary>
        public bool AllowZoom
        {
            get { return fAllowZoom; }
            set { fAllowZoom = value; }
        }

        /// <summary>
        ///   Gets or sets if the mouse can be used to pan the control.
        /// </summary>
        public bool AutoPan
        {
            get { return fAutoPan; }
            set {
                if (fAutoPan != value) {
                    fAutoPan = value;
                    if (value)
                        SizeToFit = false;
                }
            }
        }

        /// <summary>
        ///   Gets or sets the image.
        /// </summary>
        public SKImageImageSource Image
        {
            get { return fImage; }
            set {
                if (fImage != value) {
                    fImage = value;
                    AdjustLayout();
                    OnImageChanged(EventArgs.Empty);
                }
            }
        }

        public List<NamedRegion> NamedRegions
        {
            get { return fNamedRegions; }
        }

        /// <summary>
        ///   Gets or sets the selection mode.
        /// </summary>
        public ImageBoxSelectionMode SelectionMode
        {
            get { return fSelectionMode; }
            set { fSelectionMode = value; }
        }

        /// <summary>
        ///   Gets or sets the selection region.
        /// </summary>
        public Rectangle SelectionRegion
        {
            get { return fSelectionRegion; }
            set {
                if (fSelectionRegion != value) {
                    fSelectionRegion = value;
                    InvalidateContent();
                }
            }
        }

        public bool ShowNamedRegionTips
        {
            get { return fShowNamedRegionTips; }
            set { fShowNamedRegionTips = value; }
        }

        /// <summary>
        ///   Gets or sets a value indicating whether the control should automatically size to fit the image contents.
        /// </summary>
        public bool SizeToFit
        {
            get { return fSizeToFit; }
            set {
                if (fSizeToFit != value) {
                    fSizeToFit = value;
                    AdjustLayout();

                    if (value)
                        AutoPan = false;
                    else
                        ActualSize();
                }
            }
        }

        /// <summary>
        ///   Gets or sets the zoom.
        /// </summary>
        public new int Zoom
        {
            get { return fZoom; }
            set {
                if (value < ImageBoxConstants.MIN_ZOOM)
                    value = ImageBoxConstants.MIN_ZOOM;
                else if (value > ImageBoxConstants.MAX_ZOOM)
                    value = ImageBoxConstants.MAX_ZOOM;

                if (fZoom != value) {
                    fZoom = value;

                    AdjustLayout();
                    OnZoomChanged(EventArgs.Empty);
                }
            }
        }

        /// <summary>
        ///   Gets or sets the zoom levels.
        /// </summary>
        public List<int> ZoomLevels
        {
            get { return fZoomLevels; }
        }


        public ImageBox()
        {
            BackgroundColor = Color.Gray;
            Padding = new Thickness(0);

            fNamedRegions = new List<NamedRegion>();
            fZoomLevels = new List<int>(new[] { 7, 10, 15, 20, 25, 30, 50, 70, 100, 150, 200, 300, 400, 500, 600, 700, 800, 1200, 1600 });

            fAllowZoom = true;
            fAutoPan = true;
            fDropShadowSize = 3;
            fImageBorderColor = Color.AliceBlue;
            fImagePadding = new Thickness(10);
            fSelectionColor = Color.LightBlue;
            fZoom = 100;
        }

        private bool AllowPainting()
        {
            return fUpdateCount == 0;
        }

        /// <summary>
        ///   Resets the zoom to 100%.
        /// </summary>
        public void ActualSize()
        {
            if (fSizeToFit)
                SizeToFit = false;

            Zoom = 100;
        }

        /// <summary>
        ///   Disables any redrawing of the image box
        /// </summary>
        public void BeginUpdate()
        {
            fUpdateCount++;
        }

        /// <summary>
        ///   Enables the redrawing of the image box
        /// </summary>
        public void EndUpdate()
        {
            if (fUpdateCount > 0)
                fUpdateCount--;

            if (AllowPainting())
                Invalidate();
        }

        /// <summary>
        ///   Scrolls the control to the given point in the image, offset at the specified display point
        /// </summary>
        /// <param name="imageLocation">The point of the image to attempt to scroll to.</param>
        /// <param name="relativeDisplayPoint">The relative display point to offset scrolling by.</param>
        public void ScrollTo(Point imageLocation, Point relativeDisplayPoint)
        {
        }

        public void Invalidate()
        {
            InvalidateContent();
        }

        /// <summary>
        ///   Zooms into the image.
        /// </summary>
        public void ZoomIn()
        {
            if (fSizeToFit) {
                int previousZoom = fZoom;
                SizeToFit = false;
                Zoom = previousZoom; // Stop the zoom getting reset to 100% before calculating the new zoom
            }

            Zoom = NextZoom(fZoom);
        }

        /// <summary>
        ///   Zooms out of the image.
        /// </summary>
        public void ZoomOut()
        {
            if (fSizeToFit) {
                int previousZoom = fZoom;
                SizeToFit = false;
                Zoom = previousZoom; // Stop the zoom getting reset to 100% before calculating the new zoom
            }

            Zoom = PreviousZoom(fZoom);
        }

        /// <summary>
        ///   Zooms to the maximum size for displaying the entire image within the bounds of the control.
        /// </summary>
        public void ZoomToFit()
        {
            var innerRectangle = base.Viewport;
            if (fImageSize.IsZero || innerRectangle.IsEmpty()) return;

            double aspectRatio = GfxHelper.ZoomToFit(fImage.Image.Width, fImage.Image.Height, (float)(innerRectangle.Width - fImagePadding.HorizontalThickness), (float)(innerRectangle.Height - fImagePadding.VerticalThickness));
            double zoom = aspectRatio * 100.0;

            Zoom = (int)Math.Round(Math.Floor(zoom));
        }

        /// <summary>
        ///   Adjusts the view port to fit the given region.
        /// </summary>
        /// <param name="rectangle">The rectangle to fit the view port to.</param>
        public void ZoomToRegion(Rectangle rectangle)
        {
            var clientSize = new Size(Viewport.Width, Viewport.Height);
            double ratioX = clientSize.Width / rectangle.Width;
            double ratioY = clientSize.Height / rectangle.Height;
            double zoomFactor = Math.Min(ratioX, ratioY);
            int cx = (int)(rectangle.X + (rectangle.Width / 2));
            int cy = (int)(rectangle.Y + (rectangle.Height / 2));

            Zoom = (int)(zoomFactor * 100);

            // Centers the given point in the image in the center of the control
            ScrollTo(new Point(cx, cy), new Point(Viewport.Width / 2, Viewport.Height / 2));
        }

        private void AdjustLayout()
        {
            var clientSize = ClientRectangle.Size;
            if (clientSize.Height <= 0 || clientSize.Width <= 0)
                return;

            fImageSize = (fImage != null) ? new Size(fImage.Image.Width, fImage.Image.Height) : Size.Zero;
            fZoomFactor = fZoom / 100.0f;

            fScaledImageHeight = (int)(fImageSize.Height * fZoomFactor);
            fScaledImageWidth = (int)(fImageSize.Width * fZoomFactor);

            if (fSizeToFit) {
                ZoomToFit();
            } else if (!fImageSize.IsZero) {
                SetImageSize(new ExtSize(fScaledImageWidth, fScaledImageHeight), true);
            }

            InvalidateContent();
        }

        private void DrawImageBorder(Graphics gfx)
        {
        }

        protected override void OnPaint(SKPaintSurfaceEventArgs e)
        {
            var info = e.Info;

            if (!AllowPainting()) return;

            Graphics gfx = e.Surface.Canvas;
            gfx.Clear(BackgroundColor.ToSKColor());

            // draw the image
            if (fImage != null) {
                DrawImageBorder(gfx);

                var sourRect = SKRect.Create(0, 0, fImage.Image.Width, fImage.Image.Height);
                var destRect = SKRect.Create((float)ImageRect.X, (float)ImageRect.Y, (float)ImageRect.Width, (float)ImageRect.Height);
                gfx.DrawImage(fImage, sourRect, destRect);

                // draw the selection
                if (fSelectionRegion != Rectangle.Zero) {
                }
            }
        }

        private void OnSizeChanged(object sender, EventArgs e)
        {
            AdjustLayout();
        }

        private void OnImageChanged(EventArgs e)
        {
            EventHandler handler = ImageChanged;
            if (handler != null)
                handler(this, e);
        }

        private void OnZoomChanged(EventArgs e)
        {
            EventHandler handler = ZoomChanged;
            if (handler != null)
                handler(this, e);
        }


        private int FindNearestZoom(int zoomLevel)
        {
            int min = int.MaxValue;
            int minVal = 0;

            int size = fZoomLevels.Count;
            if (size != 0) {
                for (int i = 0; i < size; i++) {
                    int val = fZoomLevels[i];
                    int d = Math.Abs(val - zoomLevel);
                    if (min > d) {
                        min = d;
                        minVal = val;
                    }
                }
            }

            return minVal;
        }

        private int NextZoom(int zoomLevel)
        {
            int index = fZoomLevels.IndexOf(FindNearestZoom(zoomLevel));
            if (index < fZoomLevels.Count - 1)
                index++;

            return fZoomLevels[index];
        }

        private int PreviousZoom(int zoomLevel)
        {
            int index = fZoomLevels.IndexOf(FindNearestZoom(zoomLevel));
            if (index > 0)
                index--;

            return fZoomLevels[index];
        }
    }
}
