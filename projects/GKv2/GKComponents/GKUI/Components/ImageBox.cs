/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

namespace GKUI.Components
{
    /// <summary>
    ///   Specifies the selection mode.
    /// </summary>
    public enum ImageBoxSelectionMode
    {
        /// <summary>
        ///   No selection.
        /// </summary>
        None,

        /// <summary>
        ///   Rectangle selection.
        /// </summary>
        Rectangle,

        /// <summary>
        ///   Zoom selection.
        /// </summary>
        Zoom
    }

    /// <summary>
    ///   Specifies the border styles of an image
    /// </summary>
    public enum ImageBoxBorderStyle
    {
        /// <summary>
        ///   No border.
        /// </summary>
        None,

        /// <summary>
        ///   A fixed, single-line border.
        /// </summary>
        FixedSingle,

        /// <summary>
        ///   A fixed, single-line border with a solid drop shadow.
        /// </summary>
        FixedSingleDropShadow,

        /// <summary>
        ///   A fixed, single-line border with a soft outer glow.
        /// </summary>
        FixedSingleGlowShadow
    }

    public class NamedRegion
    {
        public readonly string Name;
        public readonly ExtRect Region;

        public NamedRegion(string name, ExtRect region)
        {
            Name = name;
            Region = region;
        }
    }

    /// <summary>
    ///   Component for displaying images with support for scrolling and zooming.
    /// </summary>
    public sealed class ImageBox : ScrollablePanel
    {
        #region Constants

        private const int MAX_ZOOM = 3500;
        private const int MIN_ZOOM = 1;
        private const int SELECTION_DEAD_ZONE = 5;

        #endregion

        #region Instance Fields

        private readonly List<NamedRegion> fNamedRegions;
        private readonly ToolTip fToolTip;
        private readonly List<int> fZoomLevels;

        private bool fAllowZoom;
        private bool fAutoPan;
        private int fDropShadowSize;
        private Image fImage;
        private Color fImageBorderColor;
        private ImageBoxBorderStyle fImageBorderStyle;
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

        #endregion

        #region Events

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

        #endregion

        #region Properties

        /// <summary>
        ///   Gets or sets a value indicating whether the user can change the zoom level.
        /// </summary>
        /// <value>
        ///   <c>true</c> if the zoom level can be changed; otherwise, <c>false</c>.
        /// </value>
        public bool AllowZoom
        {
            get { return fAllowZoom; }
            set { fAllowZoom = value; }
        }

        /// <summary>
        ///   Gets or sets if the mouse can be used to pan the control.
        /// </summary>
        /// <value>
        ///   <c>true</c> if the control can be auto panned; otherwise, <c>false</c>.
        /// </value>
        /// <remarks>If this property is set, the SizeToFit property cannot be used.</remarks>
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
        ///   Gets or sets the size of the drop shadow.
        /// </summary>
        /// <value>The size of the drop shadow.</value>
        public int DropShadowSize
        {
            get { return fDropShadowSize; }
            set {
                if (fDropShadowSize != value) {
                    fDropShadowSize = value;
                    Invalidate();
                }
            }
        }

        /// <summary>
        ///   Gets or sets the image.
        /// </summary>
        /// <value>The image.</value>
        public Image Image
        {
            get { return fImage; }
            set {
                if (fImage != value) {
                    fImage = value;
                    UpdateParams();
                    OnImageChanged(EventArgs.Empty);
                }
            }
        }

        /// <summary>
        ///   Gets or sets the color of the image border.
        /// </summary>
        /// <value>The color of the image border.</value>
        public Color ImageBorderColor
        {
            get { return fImageBorderColor; }
            set {
                if (fImageBorderColor != value) {
                    fImageBorderColor = value;
                    Invalidate();
                }
            }
        }

        /// <summary>
        ///   Gets or sets the image border style.
        /// </summary>
        /// <value>The image border style.</value>
        public ImageBoxBorderStyle ImageBorderStyle
        {
            get { return fImageBorderStyle; }
            set {
                if (fImageBorderStyle != value) {
                    fImageBorderStyle = value;
                    Invalidate();
                }
            }
        }

        /// <summary>
        ///   Gets a value indicating whether this control is panning.
        /// </summary>
        /// <value>
        ///   <c>true</c> if this control is panning; otherwise, <c>false</c>.
        /// </value>
        public bool IsPanning
        {
            get { return fIsPanning; }
            set {
                if (fIsPanning != value) {
                    fIsPanning = value;

                    if (value) {
                        fStartScrollPosition = AutoScrollPosition;
                        Cursor = Cursors.SizeAll;
                    } else Cursor = Cursors.Default;
                }
            }
        }

        /// <summary>
        ///   Gets or sets a value indicating whether this a selection region is currently being drawn.
        /// </summary>
        /// <value>
        ///   <c>true</c> if a selection region is currently being drawn; otherwise, <c>false</c>.
        /// </value>
        public bool IsSelecting
        {
            get { return fIsSelecting; }
            set {
                if (fIsSelecting != value) {
                    if (!value) {
                        if (fSelectionMode == ImageBoxSelectionMode.Zoom) {
                            if (fSelectionRegion.Width > SELECTION_DEAD_ZONE && fSelectionRegion.Height > SELECTION_DEAD_ZONE) {
                                ZoomToRegion(fSelectionRegion);
                                SelectionRegion = RectangleF.Empty;
                            }
                        }
                    }
                    fIsSelecting = value;
                }
            }
        }

        public List<NamedRegion> NamedRegions
        {
            get { return fNamedRegions; }
        }

        /// <summary>
        ///   Gets or sets the color of selection regions.
        /// </summary>
        /// <value>
        ///   The color of selection regions.
        /// </value>
        public Color SelectionColor
        {
            get { return fSelectionColor; }
            set { fSelectionColor = value; }
        }

        /// <summary>
        ///   Gets or sets the selection mode.
        /// </summary>
        /// <value>
        ///   The selection mode.
        /// </value>
        public ImageBoxSelectionMode SelectionMode
        {
            get { return fSelectionMode; }
            set { fSelectionMode = value; }
        }

        /// <summary>
        ///   Gets or sets the selection region.
        /// </summary>
        /// <value>
        ///   The selection region.
        /// </value>
        public RectangleF SelectionRegion
        {
            get { return fSelectionRegion; }
            set {
                if (fSelectionRegion != value) {
                    fSelectionRegion = value;
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
        /// <value>
        ///   <c>true</c> if the control should size to fit the image contents; otherwise, <c>false</c>.
        /// </value>
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
        /// <value>The zoom.</value>
        public int Zoom
        {
            get { return fZoom; }
            set {
                if (value < MIN_ZOOM)
                    value = MIN_ZOOM;
                else if (value > MAX_ZOOM)
                    value = MAX_ZOOM;

                if (fZoom != value) {
                    fZoom = value;

                    UpdateParams();
                    OnZoomChanged(EventArgs.Empty);
                }
            }
        }

        /// <summary>
        ///   Gets or sets the zoom levels.
        /// </summary>
        /// <value>The zoom levels.</value>
        public List<int> ZoomLevels
        {
            get { return fZoomLevels; }
        }

        #endregion

        #region Constructors

        /// <summary>
        ///   Initializes a new instance of the <see cref="ImageBox" /> class.
        /// </summary>
        public ImageBox()
        {
            SetStyle(ControlStyles.StandardDoubleClick, false);
            SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint |
                     ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            fNamedRegions = new List<NamedRegion>();
            fZoomLevels = new List<int>(new[] { 7, 10, 15, 20, 25, 30, 50, 70, 100, 150, 200, 300, 400, 500, 600, 700, 800, 1200, 1600 });

            AllowZoom = true;
            AutoSize = false;
            AutoScroll = true;
            AutoPan = true;
            BackColor = Color.White;
            DropShadowSize = 3;
            ImageBorderColor = SystemColors.ControlDark;
            ImageBorderStyle = ImageBoxBorderStyle.None;
            Padding = new Padding(0);
            SelectionColor = SystemColors.Highlight;

            fToolTip = new ToolTip();
            fToolTip.AutoPopDelay = 5000;
            fToolTip.InitialDelay = 250;
            fToolTip.ReshowDelay = 50;
            fToolTip.ShowAlways = true;

            ActualSize();
        }

        #endregion

        #region Members

        /// <summary>
        ///   Gets a value indicating whether painting of the control is allowed.
        /// </summary>
        /// <value>
        ///   <c>true</c> if painting of the control is allowed; otherwise, <c>false</c>.
        /// </value>
        private bool AllowPainting()
        {
            return fUpdateCount == 0;
        }

        private void UpdateParams()
        {
            fImageSize = (fImage != null) ? fImage.Size : Size.Empty;
            fZoomFactor = fZoom / 100.0f;

            fScaledImageHeight = (int)(fImageSize.Height * fZoomFactor);
            fScaledImageWidth = (int)(fImageSize.Width * fZoomFactor);
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
        ///   Centers the given point in the image in the center of the control
        /// </summary>
        /// <param name="imageLocation">The point of the image to attempt to center.</param>
        public void CenterAt(int x, int y)
        {
            ScrollTo(new Point(x, y), new Point(ClientSize.Width / 2, ClientSize.Height / 2));
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
        ///   Gets the image view port.
        /// </summary>
        /// <returns></returns>
        private Rectangle GetImageViewport()
        {
            Rectangle viewPort;

            if (!fImageSize.IsEmpty)
            {
                Rectangle innerRectangle = GetInsideViewport();

                if (!HScroll && !VScroll) {
                    // if no scrolling is present, tinker the view port so that the image and any applicable borders all fit inside
                    int borderOffset = -GetImageBorderOffset();
                    innerRectangle.Inflate(borderOffset, borderOffset);
                }

                int x = !HScroll ? (innerRectangle.Width - fScaledImageWidth) / 2 : 0;
                int y = !VScroll ? (innerRectangle.Height - fScaledImageHeight) / 2 : 0;
                int width = Math.Min(fScaledImageWidth - Math.Abs(AutoScrollPosition.X), innerRectangle.Width);
                int height = Math.Min(fScaledImageHeight - Math.Abs(AutoScrollPosition.Y), innerRectangle.Height);

                viewPort = new Rectangle(x + innerRectangle.Left, y + innerRectangle.Top, width, height);
            }
            else
                viewPort = Rectangle.Empty;

            return viewPort;
        }

        /// <summary>
        ///   Gets the inside view port.
        /// </summary>
        /// <param name="includePadding">
        ///   if set to <c>true</c> [include padding].
        /// </param>
        /// <returns></returns>
        private Rectangle GetInsideViewport()
        {
            Size clientSize = ClientSize;
            return new Rectangle(0, 0, clientSize.Width, clientSize.Height);
        }

        private RectangleF GetOffsetRectangle(ExtRect source)
        {
            return GetOffsetRectangle(UIHelper.Rt2Rt(source));
        }

        /// <summary>
        ///   Returns the source <see cref="T:System.Drawing.RectangleF" /> scaled according to the current zoom level and repositioned to include the current image offset
        /// </summary>
        /// <param name="source">The source.</param>
        /// <returns>A Rectangle which has been resized and repositioned to match the current zoom level and image offset</returns>
        private RectangleF GetOffsetRectangle(RectangleF source)
        {
            RectangleF viewport = GetImageViewport();
            float offsetX = viewport.Left + Padding.Left + AutoScrollPosition.X;
            float offsetY = viewport.Top + Padding.Top + AutoScrollPosition.Y;

            RectangleF scaledRect = new RectangleF(
                (source.Left * fZoomFactor), (source.Top * fZoomFactor),
                (source.Width * fZoomFactor), (source.Height * fZoomFactor));

            return new RectangleF(scaledRect.Left + offsetX, scaledRect.Top + offsetY, scaledRect.Width, scaledRect.Height);
        }

        /// <summary>
        ///   Gets the source image region.
        /// </summary>
        /// <returns></returns>
        private RectangleF GetSourceImageRegion()
        {
            RectangleF region;

            if (!fImageSize.IsEmpty)
            {
                Rectangle viewPort = GetImageViewport();
                float sourceLeft = (-AutoScrollPosition.X / fZoomFactor);
                float sourceTop = (-AutoScrollPosition.Y / fZoomFactor);
                float sourceWidth = (viewPort.Width / fZoomFactor);
                float sourceHeight = (viewPort.Height / fZoomFactor);

                region = new RectangleF(sourceLeft, sourceTop, sourceWidth, sourceHeight);
            }
            else
                region = RectangleF.Empty;

            return region;
        }

        /// <summary>
        ///   Converts the given client size point to represent a coordinate on the source image.
        /// </summary>
        /// <param name="point">The source point.</param>
        /// <param name="fitToBounds">
        ///   if set to <c>true</c> and the point is outside the bounds of the source image, it will be mapped to the nearest edge.
        /// </param>
        /// <returns>Point.Empty is the point could not be matched to the source image, otherwise the new translated point</returns>
        /// <remarks>If a match is made, the return will be offset by 1</remarks>
        private Point PointToImage(Point point, bool fitToBounds)
        {
            int x, y;

            Rectangle viewport = GetImageViewport();
            if (viewport.Contains(point) || fitToBounds)
            {
                if (AutoScrollPosition != Point.Empty)
                    point = new Point(point.X - AutoScrollPosition.X, point.Y - AutoScrollPosition.Y);

                x = (int)((point.X - viewport.X) / fZoomFactor);
                y = (int)((point.Y - viewport.Y) / fZoomFactor);

                if (x < 0)
                    x = 0;
                else if (x > fImageSize.Width)
                    x = fImageSize.Width;

                if (y < 0)
                    y = 0;
                else if (y > fImageSize.Height)
                    y = fImageSize.Height;
            }
            else
            {
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
        ///   Creates a selection region which encompasses the entire image
        /// </summary>
        /// <exception cref="System.InvalidOperationException">Thrown if no image is currently set</exception>
        public void SelectAll()
        {
            if (fImage == null)
                throw new InvalidOperationException("No image set");

            SelectionRegion = new RectangleF(PointF.Empty, fImage.Size);
        }

        /// <summary>
        ///   Clears any existing selection region
        /// </summary>
        public void SelectNone()
        {
            SelectionRegion = RectangleF.Empty;
        }

        /// <summary>
        ///   zooms into the image
        /// </summary>
        public void ZoomIn()
        {
            if (fSizeToFit)
            {
                int previousZoom = fZoom;
                SizeToFit = false;
                Zoom = previousZoom; // Stop the zoom getting reset to 100% before calculating the new zoom
            }

            Zoom = NextZoom(fZoom);
        }

        /// <summary>
        ///   Zooms out of the image
        /// </summary>
        public void ZoomOut()
        {
            if (fSizeToFit)
            {
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

            Rectangle innerRectangle = GetInsideViewport();
            double aspectRatio = GfxHelper.ZoomToFit(fImage.Width, fImage.Height, innerRectangle.Width, innerRectangle.Height);
            double zoom = aspectRatio * 100.0;

            Zoom = (int)Math.Round(Math.Floor(zoom));
        }

        /// <summary>
        ///   Adjusts the view port to fit the given region.
        /// </summary>
        /// <param name="rectangle">The rectangle to fit the view port to.</param>
        public void ZoomToRegion(RectangleF rectangle)
        {
            double ratioX = ClientSize.Width / rectangle.Width;
            double ratioY = ClientSize.Height / rectangle.Height;
            double zoomFactor = Math.Min(ratioX, ratioY);
            int cx = (int)(rectangle.X + (rectangle.Width / 2));
            int cy = (int)(rectangle.Y + (rectangle.Height / 2));

            Zoom = (int)(zoomFactor * 100);
            CenterAt(cx, cy);
        }

        /// <summary>
        ///   Adjusts the layout.
        /// </summary>
        private void AdjustLayout()
        {
            if (fSizeToFit) {
                ZoomToFit();
            } else if (!fImageSize.IsEmpty) {
                AutoScrollMinSize = new Size(fScaledImageWidth + Padding.Horizontal, fScaledImageHeight + Padding.Vertical);
            }

            Invalidate();
        }

        /// <summary>
        ///   Draws a drop shadow.
        /// </summary>
        /// <param name="g">The graphics. </param>
        /// <param name="viewPort"> The view port. </param>
        private void DrawDropShadow(Graphics g, Rectangle viewPort)
        {
            var rightEdge = new Rectangle(viewPort.Right + 1, viewPort.Top + fDropShadowSize, fDropShadowSize, viewPort.Height);
            var bottomEdge = new Rectangle(viewPort.Left + fDropShadowSize, viewPort.Bottom + 1, viewPort.Width + 1, fDropShadowSize);

            using (Brush brush = new SolidBrush(fImageBorderColor))
                g.FillRectangles(brush, new[] { rightEdge, bottomEdge });
        }

        /// <summary>
        ///   Draws a glow shadow.
        /// </summary>
        /// <param name="g">The graphics.</param>
        /// <param name="viewPort">The view port.</param>
        private void DrawGlowShadow(Graphics g, Rectangle viewPort)
        {
            g.SetClip(viewPort, CombineMode.Exclude); // make sure the inside glow doesn't appear

            using (var path = new GraphicsPath())
            {
                path.AddRectangle(viewPort);

                int glowSize = fDropShadowSize * 3;
                const int feather = 50;

                for (int i = 1; i <= glowSize; i += 2)
                {
                    int alpha = feather - ((feather / glowSize) * i);

                    using (var pen = new Pen(Color.FromArgb(alpha, fImageBorderColor), i) { LineJoin = LineJoin.Round })
                        g.DrawPath(pen, path);
                }
            }
        }

        /// <summary>
        ///   Draws a border around the image.
        /// </summary>
        /// <param name="graphics"> The graphics. </param>
        private void DrawImageBorder(Graphics graphics)
        {
            if (fImageBorderStyle == ImageBoxBorderStyle.None) return;

            graphics.SetClip(GetInsideViewport()); // make sure the image border doesn't overwrite the control border

            Rectangle viewPort = GetImageViewport();
            viewPort = new Rectangle(viewPort.Left - 1, viewPort.Top - 1, viewPort.Width + 1, viewPort.Height + 1);

            using (var borderPen = new Pen(fImageBorderColor))
                graphics.DrawRectangle(borderPen, viewPort);

            switch (fImageBorderStyle)
            {
                case ImageBoxBorderStyle.FixedSingleDropShadow:
                    DrawDropShadow(graphics, viewPort);
                    break;

                case ImageBoxBorderStyle.FixedSingleGlowShadow:
                    DrawGlowShadow(graphics, viewPort);
                    break;
            }

            graphics.ResetClip();
        }

        /// <summary>
        ///   Draws the selection region.
        /// </summary>
        /// <param name="e">
        ///   The <see cref="System.Windows.Forms.PaintEventArgs" /> instance containing the event data.
        /// </param>
        private void DrawSelection(PaintEventArgs e)
        {
            RectangleF rect = GetOffsetRectangle(fSelectionRegion);

            using (Brush brush = new SolidBrush(Color.FromArgb(128, fSelectionColor)))
                e.Graphics.FillRectangle(brush, rect);

            using (var pen = new Pen(fSelectionColor))
                e.Graphics.DrawRectangle(pen, rect.X, rect.Y, rect.Width, rect.Height);
        }

        private void DrawNamedRegions(Graphics gfx)
        {
            foreach (var region in fNamedRegions) {
                RectangleF rect = GetOffsetRectangle(region.Region);

                using (Brush brush = new SolidBrush(Color.FromArgb(128, fSelectionColor)))
                    gfx.FillRectangle(brush, rect);

                using (var pen = new Pen(fSelectionColor))
                    gfx.DrawRectangle(pen, rect.X, rect.Y, rect.Width, rect.Height);
            }
        }

        /// <summary>
        ///   Gets an offset based on the current image border style.
        /// </summary>
        /// <returns></returns>
        private int GetImageBorderOffset()
        {
            int offset;

            switch (fImageBorderStyle)
            {
                case ImageBoxBorderStyle.FixedSingle:
                    offset = 1;
                    break;

                case ImageBoxBorderStyle.FixedSingleDropShadow:
                    offset = (fDropShadowSize + 1);
                    break;

                default:
                    offset = 0;
                    break;
            }

            return offset;
        }

        #region Overriden methods

        /// <summary>
        ///   Determines whether the specified key is a regular input key or a special key that requires preprocessing.
        /// </summary>
        /// <param name="keyData">
        ///   One of the <see cref="T:System.Windows.Forms.Keys" /> values.
        /// </param>
        /// <returns>
        ///   true if the specified key is a regular input key; otherwise, false.
        /// </returns>
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

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.Control.BackColorChanged" /> event.
        /// </summary>
        /// <param name="e">
        ///   An <see cref="T:System.EventArgs" /> that contains the event data.
        /// </param>
        protected override void OnBackColorChanged(EventArgs e)
        {
            base.OnBackColorChanged(e);
            Invalidate();
        }

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.Control.KeyDown" /> event.
        /// </summary>
        /// <param name="e">
        ///   A <see cref="T:System.Windows.Forms.KeyEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnKeyDown(KeyEventArgs e)
        {
            base.OnKeyDown(e);

            ProcessScrollingShortcuts(e);
            ProcessImageShortcuts(e);
        }

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.Control.MouseDown" /> event.
        /// </summary>
        /// <param name="e">
        ///   A <see cref="T:System.Windows.Forms.MouseEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (!Focused)
                Focus();

            if (e.Button == MouseButtons.Left && fSelectionMode != ImageBoxSelectionMode.None)
                SelectionRegion = Rectangle.Empty;
        }

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.Control.MouseMove" /> event.
        /// </summary>
        /// <param name="e">
        ///   A <see cref="T:System.Windows.Forms.MouseEventArgs" /> that contains the event data.
        /// </param>
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
                            RectangleF rect = GetOffsetRectangle(region.Region);
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

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.Control.MouseUp" /> event.
        /// </summary>
        /// <param name="e">
        ///   A <see cref="T:System.Windows.Forms.MouseEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseUp(e);

            if (fIsPanning)
                IsPanning = false;

            if (fIsSelecting)
                IsSelecting = false;
        }

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.Control.MouseWheel" /> event.
        /// </summary>
        /// <param name="e">
        ///   A <see cref="T:System.Windows.Forms.MouseEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnMouseWheel(MouseEventArgs e)
        {
            base.OnMouseWheel(e);

            if (fAllowZoom && !fSizeToFit)
                ProcessMouseZoom(e.Delta > 0, e.Location);
        }

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.Control.Paint" /> event.
        /// </summary>
        /// <param name="e">
        ///   A <see cref="T:System.Windows.Forms.PaintEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnPaint(PaintEventArgs e)
        {
            if (!AllowPainting()) return;

            Graphics gfx = e.Graphics;

            Rectangle innerRectangle = GetInsideViewport();

            // draw the background
            using (var brush = new SolidBrush(BackColor))
                gfx.FillRectangle(brush, innerRectangle);

            // draw the image
            if (!fImageSize.IsEmpty)
                DrawImageBorder(gfx);

            if (fImage != null) {
                gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
                gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
                gfx.DrawImage(fImage, GetImageViewport(), GetSourceImageRegion(), GraphicsUnit.Pixel);
            }

            gfx.SetClip(GetInsideViewport());

            // draw the selection
            if (fSelectionRegion != Rectangle.Empty)
                DrawSelection(e);

            DrawNamedRegions(gfx);

            gfx.ResetClip();

            base.OnPaint(e);
        }

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.Control.Resize" /> event.
        /// </summary>
        /// <param name="e">
        ///   An <see cref="T:System.EventArgs" /> that contains the event data.
        /// </param>
        protected override void OnResize(EventArgs e)
        {
            AdjustLayout();
            base.OnResize(e);
        }

        #endregion

        /// <summary>
        ///   Raises the <see cref="ImageChanged" /> event.
        /// </summary>
        /// <param name="e">
        ///   The <see cref="System.EventArgs" /> instance containing the event data.
        /// </param>
        private void OnImageChanged(EventArgs e)
        {
            AdjustLayout();

            EventHandler handler = ImageChanged;
            if (handler != null)
                handler(this, e);
        }

        /// <summary>
        ///   Raises the <see cref="SelectionRegionChanged" /> event.
        /// </summary>
        /// <param name="e">
        ///   The <see cref="System.EventArgs" /> instance containing the event data.
        /// </param>
        private void OnSelectionRegionChanged(EventArgs e)
        {
            Invalidate();

            EventHandler handler = SelectionRegionChanged;
            if (handler != null)
                handler(this, e);
        }

        /// <summary>
        ///   Raises the <see cref="ZoomChanged" /> event.
        /// </summary>
        /// <param name="e">
        ///   The <see cref="System.EventArgs" /> instance containing the event data.
        /// </param>
        private void OnZoomChanged(EventArgs e)
        {
            AdjustLayout();

            EventHandler handler = ZoomChanged;
            if (handler != null)
                handler(this, e);
        }

        /// <summary>
        ///   Processes shortcut keys for zooming and selection
        /// </summary>
        /// <param name="e">
        ///   The <see cref="KeyEventArgs" /> instance containing the event data.
        /// </param>
        private void ProcessImageShortcuts(KeyEventArgs e)
        {
            int previousZoom = fZoom;

            switch (e.KeyCode)
            {
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

            if (fZoom != previousZoom && !AutoScrollMinSize.IsEmpty)
                AutoScrollPosition = new Point((AutoScrollMinSize.Width - ClientSize.Width) / 2, (AutoScrollMinSize.Height - ClientSize.Height) / 2);
        }

        /// <summary>
        ///   Processes zooming with the mouse. Attempts to keep the pre-zoom image pixel under the mouse after the zoom has completed.
        /// </summary>
        /// <param name="isZoomIn">
        ///   if set to <c>true</c> zoom in, otherwise zoom out.
        /// </param>
        /// <param name="cursorPosition">The cursor position.</param>
        private void ProcessMouseZoom(bool isZoomIn, Point cursorPosition)
        {
            Point currentPixel = PointToImage(cursorPosition, false);
            int currentZoom = fZoom;

            Zoom = isZoomIn ? NextZoom(fZoom) : PreviousZoom(fZoom);

            if (fZoom != currentZoom)
                ScrollTo(currentPixel, cursorPosition);
        }

        /// <summary>
        ///   Performs mouse based panning
        /// </summary>
        /// <param name="e">
        ///   The <see cref="MouseEventArgs" /> instance containing the event data.
        /// </param>
        /// <param name="selectionMode">
        /// </param>
        private void ProcessPanning(MouseEventArgs e, ImageBoxSelectionMode selectionMode)
        {
            if (fAutoPan && !fImageSize.IsEmpty && selectionMode == ImageBoxSelectionMode.None)
            {
                if (!fIsPanning && (HScroll || VScroll))
                {
                    fStartMousePosition = e.Location;
                    IsPanning = true;
                }

                if (fIsPanning)
                {
                    int x = -fStartScrollPosition.X + (fStartMousePosition.X - e.Location.X);
                    int y = -fStartScrollPosition.Y + (fStartMousePosition.Y - e.Location.Y);

                    UpdateScrollPosition(x, y);
                }
            }
        }

        /// <summary>
        ///   Processes shortcut keys for scrolling
        /// </summary>
        /// <param name="e">
        ///   The <see cref="KeyEventArgs" /> instance containing the event data.
        /// </param>
        private void ProcessScrollingShortcuts(KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
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
        }

        /// <summary>
        ///   Performs mouse based region selection
        /// </summary>
        /// <param name="e">
        ///   The <see cref="MouseEventArgs" /> instance containing the event data.
        /// </param>
        private void ProcessSelection(MouseEventArgs e)
        {
            if (fSelectionMode == ImageBoxSelectionMode.None) return;

            Point mpt = e.Location;
            if (!fIsSelecting)
            {
                fStartMousePosition = mpt;
                IsSelecting = true;
                return;
            }

            float x, y, w, h;

            if (mpt.X < fStartMousePosition.X)
            {
                x = mpt.X;
                w = fStartMousePosition.X - mpt.X;
            }
            else
            {
                x = fStartMousePosition.X;
                w = mpt.X - fStartMousePosition.X;
            }

            if (mpt.Y < fStartMousePosition.Y)
            {
                y = mpt.Y;
                h = fStartMousePosition.Y - mpt.Y;
            }
            else
            {
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
        
        #endregion
    }
}
