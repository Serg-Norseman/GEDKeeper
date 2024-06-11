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
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using BSLib;
using GKCore.Types;

namespace GKUI.Components
{
    /// <summary>
    ///   Component for displaying images with support for scrolling and zooming.
    /// </summary>
    public sealed class ImageBox : ScrollablePanel
    {
        private readonly List<NamedRegion> fNamedRegions;
        private readonly ToolTip fToolTip;
        private readonly List<int> fZoomLevels;

        private bool fAllowZoom;
        private bool fAutoPan;
        private int fDropShadowSize;
        private Image fImage;
        private Color fImageBorderColor;
        private bool fIsPanning;
        private bool fIsSelecting;
        private int fScaledImageHeight;
        private int fScaledImageWidth;
        private Color fSelectionColor;
        private ImageBoxSelectionMode fSelectionMode;
        private RectangleF fSelectionRegion;
        private bool fShowNamedRegionTips;
        private bool fSizeToFit;
        private Point fStartMousePosition;
        private Point fStartScrollPosition;
        private string fTip;
        private int fUpdateCount;
        private int fZoom;
        private Size fImageSize;
        private float fZoomFactor;
        private Padding fImagePadding;


        /// <summary>
        ///   Occurs when the Image property is changed.
        /// </summary>
        public event EventHandler ImageChanged;

        /// <summary>
        ///   Occurs when the SelectionRegion property is changed.
        /// </summary>
        public event EventHandler SelectionRegionChanged;

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
        public Image Image
        {
            get { return fImage; }
            set {
                if (fImage != value) {
                    if (fImage != null) {
                        fImage.Dispose();
                    }

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
        public RectangleF SelectionRegion
        {
            get { return fSelectionRegion; }
            set {
                if (fSelectionRegion != value) {
                    fSelectionRegion = value;
                    Invalidate();
                    OnSelectionRegionChanged(EventArgs.Empty);
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
        public int Zoom
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
            SetStyle(ControlStyles.StandardDoubleClick, false);
            SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint |
                     ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            AutoSize = false;
            AutoScroll = true;
            BackColor = Color.Gray;
            Padding = new Padding(0);
            CenteredImage = true;

            fNamedRegions = new List<NamedRegion>();
            fZoomLevels = new List<int>(new[] { 7, 10, 15, 20, 25, 30, 50, 70, 100, 150, 200, 300, 400, 500, 600, 700, 800, 1200, 1600 });

            fAllowZoom = true;
            fAutoPan = true;
            fDropShadowSize = 3;
            fImageBorderColor = Color.AliceBlue;
            fImagePadding = new Padding(10);
            fSelectionColor = SystemColors.Highlight;
            fZoom = 100;

            fToolTip = new ToolTip();
            fToolTip.AutoPopDelay = 5000;
            fToolTip.InitialDelay = 250;
            fToolTip.ReshowDelay = 50;
            fToolTip.ShowAlways = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fImage != null) {
                    fImage.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        private void SetPanning(bool value)
        {
            if (fIsPanning != value) {
                fIsPanning = value;

                if (value) {
                    fStartScrollPosition = AutoScrollPosition;
                    Cursor = Cursors.SizeAll;
                } else Cursor = Cursors.Default;
            }
        }

        private void SetSelecting(bool value)
        {
            if (fIsSelecting != value) {
                if (!value && fSelectionMode == ImageBoxSelectionMode.Zoom) {
                    if (fSelectionRegion.Width > ImageBoxConstants.SELECTION_DEAD_ZONE && fSelectionRegion.Height > ImageBoxConstants.SELECTION_DEAD_ZONE) {
                        // Adjusts the view port to fit the given region
                        var clientSize = ClientSize;
                        double ratioX = clientSize.Width / fSelectionRegion.Width;
                        double ratioY = clientSize.Height / fSelectionRegion.Height;
                        double zoomFactor = Math.Min(ratioX, ratioY);
                        int cx = (int)(fSelectionRegion.X + (fSelectionRegion.Width / 2));
                        int cy = (int)(fSelectionRegion.Y + (fSelectionRegion.Height / 2));

                        Zoom = (int)(zoomFactor * 100);

                        // Centers the given point in the image in the center of the control
                        ScrollTo(new Point(cx, cy), new Point(clientSize.Width / 2, clientSize.Height / 2));

                        SelectionRegion = RectangleF.Empty;
                    }
                }
                fIsSelecting = value;
            }
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

        private Rectangle GetImageViewport()
        {
            Rectangle viewport;

            if (!fImageSize.IsEmpty) {
                Rectangle innerRectangle = GetInsideViewport();

                if (!HScroll && !VScroll) {
                    // if no scrolling is present, tinker the view port so that the image and any applicable borders all fit inside
                    int borderOffset = -GetImageBorderOffset();
                    innerRectangle.Inflate(borderOffset, borderOffset);
                }

                int x = innerRectangle.Left + (!HScroll ? (innerRectangle.Width - fScaledImageWidth) / 2 : 0);
                int y = innerRectangle.Top + (!VScroll ? (innerRectangle.Height - fScaledImageHeight) / 2 : 0);
                int width = Math.Min(fScaledImageWidth - Math.Abs(AutoScrollPosition.X), innerRectangle.Width);
                int height = Math.Min(fScaledImageHeight - Math.Abs(AutoScrollPosition.Y), innerRectangle.Height);

                viewport = new Rectangle(x, y, width, height);
            } else
                viewport = Rectangle.Empty;

            return viewport;
        }

        private Rectangle GetInsideViewport()
        {
            Size clientSize = ClientSize;
            return new Rectangle(0, 0, clientSize.Width, clientSize.Height);
        }

        private RectangleF GetOffsetRectangle(RectangleF source)
        {
            RectangleF viewport = GetImageViewport();
            float offsetX = viewport.Left + Padding.Left + AutoScrollPosition.X;
            float offsetY = viewport.Top + Padding.Top + AutoScrollPosition.Y;

            RectangleF scaledRect = new RectangleF(
                offsetX + (source.Left * fZoomFactor),
                offsetY + (source.Top * fZoomFactor),
                (source.Width * fZoomFactor),
                (source.Height * fZoomFactor));

            return scaledRect;
        }

        private RectangleF GetSourceImageRegion()
        {
            RectangleF region;

            if (!fImageSize.IsEmpty) {
                Rectangle viewPort = GetImageViewport();
                float sourceLeft = (-AutoScrollPosition.X / fZoomFactor);
                float sourceTop = (-AutoScrollPosition.Y / fZoomFactor);
                float sourceWidth = (viewPort.Width / fZoomFactor);
                float sourceHeight = (viewPort.Height / fZoomFactor);

                region = new RectangleF(sourceLeft, sourceTop, sourceWidth, sourceHeight);
            } else
                region = RectangleF.Empty;

            return region;
        }

        private Point PointToImage(Point point, bool fitToBounds)
        {
            int x, y;

            Rectangle viewport = GetImageViewport();
            if (viewport.Contains(point) || fitToBounds) {
                if (AutoScrollPosition != Point.Empty)
                    point = new Point(point.X - AutoScrollPosition.X, point.Y - AutoScrollPosition.Y);

                x = Algorithms.CheckBounds((int)((point.X - viewport.X) / fZoomFactor), 0, fImageSize.Width);
                y = Algorithms.CheckBounds((int)((point.Y - viewport.Y) / fZoomFactor), 0, fImageSize.Height);
            } else {
                // Return Point.Empty if we couldn't match
                x = 0;
                y = 0;
            }

            return new Point(x, y);
        }

        /// <summary>
        ///   Scrolls the control to the given point in the image, offset at the specified display point
        /// </summary>
        /// <param name="imageLocation">The point of the image to attempt to scroll to.</param>
        /// <param name="relativeDisplayPoint">The relative display point to offset scrolling by.</param>
        public void ScrollTo(Point imageLocation, Point relativeDisplayPoint)
        {
            int x = (int)(imageLocation.X * fZoomFactor) - relativeDisplayPoint.X;
            int y = (int)(imageLocation.Y * fZoomFactor) - relativeDisplayPoint.Y;

            UpdateScrollPosition(x, y);
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
            if (fImageSize.IsEmpty) return;

            AutoScrollMinSize = Size.Empty;

            var innerSize = base.ClientSize;
            double aspectRatio = GfxHelper.ZoomToFit(fImage.Width, fImage.Height, innerSize.Width - fImagePadding.Horizontal, innerSize.Height - fImagePadding.Vertical);
            double zoom = aspectRatio * 100.0;

            Zoom = (int)Math.Round(Math.Floor(zoom));
        }

        private void AdjustLayout()
        {
            fImageSize = (fImage != null) ? fImage.Size : Size.Empty;
            fZoomFactor = fZoom / 100.0f;

            fScaledImageHeight = (int)(fImageSize.Height * fZoomFactor);
            fScaledImageWidth = (int)(fImageSize.Width * fZoomFactor);

            if (fSizeToFit) {
                ZoomToFit();
            } else if (!fImageSize.IsEmpty) {
                SetImageSize(new ExtSize(fScaledImageWidth, fScaledImageHeight), true);
            }

            Invalidate();
        }

        private void DrawImageBorder(Graphics gfx)
        {
            Rectangle viewPort = GetImageViewport();
            viewPort = new Rectangle(viewPort.Left - 1, viewPort.Top - 1, viewPort.Width + 1, viewPort.Height + 1);

            using (var borderPen = new Pen(fImageBorderColor))
                gfx.DrawRectangle(borderPen, viewPort);

            var rightEdge = new Rectangle(viewPort.Right + 1, viewPort.Top + fDropShadowSize, fDropShadowSize, viewPort.Height);
            var bottomEdge = new Rectangle(viewPort.Left + fDropShadowSize, viewPort.Bottom + 1, viewPort.Width + 1, fDropShadowSize);

            var color = Color.FromArgb(255 / 2, fImageBorderColor);
            using (Brush brush = new SolidBrush(color))
                gfx.FillRectangles(brush, new[] { rightEdge, bottomEdge });
        }

        private int GetImageBorderOffset()
        {
            return (fDropShadowSize + 1);
        }

        protected override bool IsInputKey(Keys keyData)
        {
            bool result;

            if ((keyData & Keys.Right) == Keys.Right || (keyData & Keys.Left) == Keys.Left ||
                (keyData & Keys.Up) == Keys.Up || (keyData & Keys.Down) == Keys.Down)
                result = true;
            else
                result = base.IsInputKey(keyData);

            return result;
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            base.OnKeyDown(e);

            // Processes shortcut keys for scrolling
            switch (e.KeyCode) {
                case Keys.Left:
                    AdjustScroll(-(e.Modifiers == Keys.None ? HorizontalScroll.SmallChange : HorizontalScroll.LargeChange), 0);
                    break;

                case Keys.Right:
                    AdjustScroll(e.Modifiers == Keys.None ? HorizontalScroll.SmallChange : HorizontalScroll.LargeChange, 0);
                    break;

                case Keys.Up:
                    AdjustScroll(0, -(e.Modifiers == Keys.None ? VerticalScroll.SmallChange : VerticalScroll.LargeChange));
                    break;

                case Keys.Down:
                    AdjustScroll(0, e.Modifiers == Keys.None ? VerticalScroll.SmallChange : VerticalScroll.LargeChange);
                    break;
            }

            // Processes shortcut keys for zooming and selection
            int previousZoom = fZoom;
            switch (e.KeyCode) {
                case Keys.Home:
                    if (fAllowZoom)
                        ActualSize();
                    break;

                case Keys.PageDown:
                case Keys.Oemplus:
                    if (fAllowZoom)
                        ZoomIn();
                    break;

                case Keys.PageUp:
                case Keys.OemMinus:
                    if (fAllowZoom)
                        ZoomOut();
                    break;
            }
            if (fZoom != previousZoom && !AutoScrollMinSize.IsEmpty) {
                AutoScrollPosition = new Point((AutoScrollMinSize.Width - ClientSize.Width) / 2, (AutoScrollMinSize.Height - ClientSize.Height) / 2);
            }
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (!Focused)
                Focus();

            if (e.Button == MouseButtons.Left && fSelectionMode != ImageBoxSelectionMode.None)
                SelectionRegion = RectangleF.Empty;
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            switch (e.Button) {
                case MouseButtons.Left:
                    ProcessPanning(e, ImageBoxSelectionMode.Zoom);
                    ProcessSelection(e);
                    break;

                case MouseButtons.Right:
                    ProcessPanning(e, ImageBoxSelectionMode.None);
                    break;

                case MouseButtons.None:
                    if (fShowNamedRegionTips) {
                        string tip = "";
                        foreach (var region in fNamedRegions) {
                            RectangleF rect = GetOffsetRectangle(UIHelper.Rt2Rt(region.Region));
                            if (rect.Contains(e.Location)) {
                                tip = region.Name;
                                break;
                            }
                        }

                        if (fTip != tip) {
                            fTip = tip;
                            if (tip != "") {
                                fToolTip.Show(tip, this, e.X, e.Y, 3000);
                            } else {
                                fToolTip.Hide(this);
                            }
                        }
                    }
                    break;
            }
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseUp(e);

            if (fIsPanning)
                SetPanning(false);

            if (fIsSelecting)
                SetSelecting(false);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            base.OnMouseWheel(e);

            if (fAllowZoom && !fSizeToFit) {
                bool isZoomIn = e.Delta > 0;
                var cursorPosition = e.Location;

                Point currentPixel = PointToImage(cursorPosition, false);
                int currentZoom = fZoom;

                Zoom = isZoomIn ? NextZoom(fZoom) : PreviousZoom(fZoom);

                if (fZoom != currentZoom)
                    ScrollTo(currentPixel, cursorPosition);
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            if (!AllowPainting()) return;

            Graphics gfx = e.Graphics;
            gfx.Clear(BackColor);

            // draw the image
            if (fImage != null) {
                DrawImageBorder(gfx);

                gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
                gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
                gfx.DrawImage(fImage, GetImageViewport(), GetSourceImageRegion(), GraphicsUnit.Pixel);

                // draw the selection
                if (fSelectionRegion != Rectangle.Empty) {
                    RectangleF rect = GetOffsetRectangle(fSelectionRegion);

                    using (Brush brush = new SolidBrush(Color.FromArgb(128, fSelectionColor)))
                        gfx.FillRectangle(brush, rect);

                    using (var pen = new Pen(fSelectionColor))
                        gfx.DrawRectangle(pen, rect.X, rect.Y, rect.Width, rect.Height);
                }

                // draw named regions
                using (var pen = new Pen(fImageBorderColor, 2)) {
                    foreach (var region in fNamedRegions) {
                        RectangleF rect = GetOffsetRectangle(UIHelper.Rt2Rt(region.Region));
                        gfx.DrawRectangle(pen, rect.X, rect.Y, rect.Width, rect.Height);
                    }
                }
            }

            base.OnPaint(e);
        }

        protected override void OnResize(EventArgs e)
        {
            AdjustLayout();

            base.OnResize(e);
        }

        private void OnImageChanged(EventArgs e)
        {
            EventHandler handler = ImageChanged;
            if (handler != null)
                handler(this, e);
        }

        private void OnSelectionRegionChanged(EventArgs e)
        {
            EventHandler handler = SelectionRegionChanged;
            if (handler != null)
                handler(this, e);
        }

        private void OnZoomChanged(EventArgs e)
        {
            EventHandler handler = ZoomChanged;
            if (handler != null)
                handler(this, e);
        }

        private void ProcessPanning(MouseEventArgs e, ImageBoxSelectionMode selectionMode)
        {
            // Performs mouse based panning
            if (fAutoPan && !fImageSize.IsEmpty && selectionMode == ImageBoxSelectionMode.None) {
                if (!fIsPanning && (HScroll || VScroll)) {
                    fStartMousePosition = e.Location;
                    SetPanning(true);
                }

                if (fIsPanning) {
                    int x = -fStartScrollPosition.X + (fStartMousePosition.X - e.Location.X);
                    int y = -fStartScrollPosition.Y + (fStartMousePosition.Y - e.Location.Y);

                    UpdateScrollPosition(x, y);
                }
            }
        }

        private void ProcessSelection(MouseEventArgs e)
        {
            // Performs mouse based region selection
            if (fSelectionMode == ImageBoxSelectionMode.None) return;

            Point mpt = e.Location;
            if (!fIsSelecting) {
                fStartMousePosition = mpt;
                SetSelecting(true);
                return;
            }

            float x, y, w, h;

            if (mpt.X < fStartMousePosition.X) {
                x = mpt.X;
                w = fStartMousePosition.X - mpt.X;
            } else {
                x = fStartMousePosition.X;
                w = mpt.X - fStartMousePosition.X;
            }

            if (mpt.Y < fStartMousePosition.Y) {
                y = mpt.Y;
                h = fStartMousePosition.Y - mpt.Y;
            } else {
                y = fStartMousePosition.Y;
                h = mpt.Y - fStartMousePosition.Y;
            }

            Point imageOffset = GetImageViewport().Location;
            x = (x - imageOffset.X - AutoScrollPosition.X) / fZoomFactor;
            y = (y - imageOffset.Y - AutoScrollPosition.Y) / fZoomFactor;
            w = w / fZoomFactor;
            h = h / fZoomFactor;

            // Fits a given rectangle's coordinates to match image boundaries
            if (x < 0) x = 0;
            if (y < 0) y = 0;
            if (x + w > fImageSize.Width) w = fImageSize.Width - x;
            if (y + h > fImageSize.Height) h = fImageSize.Height - y;

            SelectionRegion = new RectangleF(x, y, w, h);
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
