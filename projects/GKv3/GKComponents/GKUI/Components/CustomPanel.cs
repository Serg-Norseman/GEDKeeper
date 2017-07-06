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
using GKCommon;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class CustomPanel : Scrollable
    {
        public const int SmallChange = 1;
        public const int LargeChange = 10;

        /*
         * Attention: In Eto, using getScrollPosition at runtime Paint causes an exception.
         * Therefore, by the time of drawing, the Viewport needs to have already been calculated.
         */

        private Drawable fCanvas;
        private Size fCanvasSize;
        private bool fCenteredImage;
        private Font fFont;
        private bool fHasScroll;
        private Color fTextColor;
        private Rectangle fViewport;


        public Size CanvasSize
        {
            get { return fCanvasSize; }
        }

        protected bool CenteredImage
        {
            get { return fCenteredImage; }
            set { fCenteredImage = value; }
        }

        public Rectangle ClientRectangle
        {
            get { return fCanvas.Bounds; }
        }

        public Font Font
        {
            get { return fFont; }
            set {
                if (fFont == value) {
                    fFont = value;
                    DoFontChanged();
                }
            }
        }

        public bool HasScroll
        {
            get { return fHasScroll; }
        }

        public new Size ScrollSize
        {
            get {
                return fCanvas.MinimumSize;
            }
            set {
                fCanvas.MinimumSize = value;
                base.UpdateScrollSizes();
            }
        }

        public Color TextColor
        {
            get { return fTextColor; }
            set { fTextColor = value; }
        }

        protected Rectangle Viewport
        {
            get {
                if (fViewport.IsEmpty) {
                    Size clientSize = ClientRectangle.Size;
                    SetViewport(new Rectangle(0, 0, clientSize.Width, clientSize.Height));
                }
                return fViewport;
            }
        }

        // FIXME: need to refactor and remove
        #region Temp for compatibility

        public int HorizontalScrollValue
        {
            get { return AutoScrollPosition.X; }
            set { UpdateScrollPosition(value, base.ScrollPosition.Y); }
        }

        public int VerticalScrollValue
        {
            get { return AutoScrollPosition.Y; }
            set { UpdateScrollPosition(base.ScrollPosition.X, value); }
        }

        public Point AutoScrollPosition
        {
            //get { return new Point(-base.ScrollPosition.X, -base.ScrollPosition.Y); }
            get { return fViewport.Location; }
        }

        #endregion


        public CustomPanel()
        {
            base.ExpandContentHeight = true;
            base.ExpandContentWidth = true;
            base.Padding = new Padding(0);

            fCanvas = new Drawable();
            fCanvas.Paint += PaintHandler;
            fCanvas.CanFocus = true;
            Content = fCanvas;

            fFont = SystemFonts.Label();
            fTextColor = Colors.Black;
        }

        protected void DoFontChanged()
        {
            OnFontChanged(EventArgs.Empty);
        }

        protected virtual void OnFontChanged(EventArgs e)
        {
        }

        private void PaintHandler(object sender, PaintEventArgs e)
        {
            OnPaint(e);
        }

        private void UpdateProperties()
        {
            fHasScroll = (fViewport.Width < fCanvasSize.Width || fViewport.Height < fCanvasSize.Height);
        }

        private void SetViewport(Rectangle viewport)
        {
            fViewport = viewport;
            UpdateProperties();
        }

        protected virtual void OnPaint(PaintEventArgs e)
        {
        }

        public Graphics CreateGraphics()
        {
            if (fCanvas.SupportsCreateGraphics) {
                return fCanvas.CreateGraphics();
            } else {
                return null;
            }
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if (!HasFocus) {
                Focus();
            }
            base.OnMouseDown(e);
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            SetViewport(VisibleRect);
            base.OnSizeChanged(e);
        }

        /// <summary>
        /// Raises the <see cref="Eto.Forms.Scrollable.Scroll" /> event.
        /// </summary>
        /// <param name="e">
        /// A <see cref="T:Eto.Forms.ScrollEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnScroll(ScrollEventArgs e)
        {
            SetViewport(VisibleRect);
            fCanvas.Invalidate();

            base.OnScroll(e);
        }

        /// <summary>
        /// Updates the scroll position.
        /// </summary>
        /// <param name="posX">The X position.</param>
        /// <param name="posY">The Y position.</param>
        protected void UpdateScrollPosition(int posX, int posY)
        {
            ScrollPosition = new Point(posX, posY);
        }

        /// <summary>
        /// Adjusts the scroll.
        /// </summary>
        /// <param name="dx">The X shift.</param>
        /// <param name="dy">The Y shift.</param>
        protected void AdjustScroll(int dx, int dy)
        {
            Point curScroll = base.ScrollPosition;
            UpdateScrollPosition(curScroll.X + dx, curScroll.Y + dy);
        }

        /// <summary>
        /// Sets the sizes of nested canvas.
        /// </summary>
        /// <param name="imageSize">The size of canvas.</param>
        /// <param name="noRedraw">Flag of the need to redraw.</param>
        protected void SetCanvasSize(ExtSize imageSize, bool noRedraw = false)
        {
            if (!imageSize.IsEmpty) {
                fCanvasSize = new Size(imageSize.Width, imageSize.Height);
                ScrollSize = fCanvasSize;
                UpdateProperties();
            }

            if (!noRedraw) Invalidate();
        }

        protected ExtRect GetImageRegion()
        {
            ExtRect imageRegion;

            if (!fCanvasSize.IsEmpty) {
                int x, y;
                if (fHasScroll) {
                    x = fViewport.Left;
                    y = fViewport.Top;
                } else {
                    if (fCenteredImage) {
                        x = (fViewport.Width - fCanvasSize.Width) / 2;
                        y = (fViewport.Height - fCanvasSize.Height) / 2;
                    } else {
                        x = 0;
                        y = 0;
                    }
                }
                int width = Math.Min(fCanvasSize.Width, fViewport.Width);
                int height = Math.Min(fCanvasSize.Height, fViewport.Height);

                imageRegion = ExtRect.CreateBounds(x, y, width, height);
            } else {
                imageRegion = ExtRect.Empty;
            }

            return imageRegion;
        }

        protected Point ConvertMouseLocation(PointF mpt)
        {
            ExtRect imRect = GetImageRegion();
            int x = (int)mpt.X;
            int y = (int)mpt.Y;

            if (fHasScroll) {
                x += imRect.Left;
                y += imRect.Top;
            } else {
                x -= imRect.Left;
                y -= imRect.Top;
            }

            return new Point(x, y);
        }
    }
}
