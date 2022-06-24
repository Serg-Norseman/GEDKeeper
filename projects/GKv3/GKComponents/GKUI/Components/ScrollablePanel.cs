/*
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

//#define DEBUG_VIEWPORT

using System;
using BSLib;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    /// <summary>
    ///
    /// </summary>
    public class ScrollablePanel : Scrollable
    {
        public const int SmallChange = 1;
        public const int LargeChange = 10;

        private Drawable fCanvas;
        private bool fCenteredImage;
        private Font fFont;
        private bool fHasHScroll;
        private bool fHasVScroll;
        private Size fImageSize;
        private Rectangle fImageRect;
        private Rectangle fImageViewport;
        private int fMouseOffsetX, fMouseOffsetY;
        private Color fTextColor;
        private Rectangle fViewport;


        public Rectangle CanvasRectangle
        {
            get { return fCanvas.Bounds; }
        }

        protected bool CenteredImage
        {
            get { return fCenteredImage; }
            set { fCenteredImage = value; }
        }

        /// <summary>
        /// Gets the rectangle that represents the client area of the control.
        /// </summary>
        public Rectangle ClientRectangle
        {
            get {
                var clientSize = base.ClientSize;
                return new Rectangle(0, 0, clientSize.Width, clientSize.Height);
            }
        }

        public Font Font
        {
            get { return fFont; }
            set {
                if (fFont != value) {
                    fFont = value;
                    OnFontChanged(EventArgs.Empty);
                }
            }
        }

        protected Rectangle ImageRect
        {
            get { return fImageRect; }
        }

        protected Rectangle ImageViewport
        {
            get { return fImageViewport; }
        }

        public bool HScroll
        {
            get { return fHasHScroll; }
        }

        public bool VScroll
        {
            get { return fHasVScroll; }
        }

        public Color TextColor
        {
            get { return fTextColor; }
            set { fTextColor = value; }
        }

        /// <summary>
        /// The rectangle that is visible to the user.
        /// </summary>
        public Rectangle Viewport
        {
            get { return fViewport; }
        }


        public ScrollablePanel()
        {
            base.ExpandContentHeight = true;
            base.ExpandContentWidth = true;
            base.Padding = new Padding(0);

            fCanvas = new Drawable();
            fCanvas.Paint += PaintHandler;
            fCanvas.KeyDown += KeyDownHandler;
            fCanvas.KeyUp += KeyUpHandler;
            fCanvas.CanFocus = true;
            Content = fCanvas;

            fFont = SystemFonts.Label();
            fTextColor = SystemColors.ControlText;

            SetImageSize(new ExtSize(100, 100), false);
        }

        private void KeyDownHandler(object sender, KeyEventArgs e)
        {
            OnKeyDown(e);
        }

        private void KeyUpHandler(object sender, KeyEventArgs e)
        {
            OnKeyUp(e);
        }

        protected virtual void OnFontChanged(EventArgs e)
        {
        }

        private void PaintHandler(object sender, PaintEventArgs e)
        {
            OnPaint(e);

            #if DEBUG_VIEWPORT
            var gfx = e.Graphics;
            using (var pen = new Pen(Colors.Red, 1.0f)) {
                gfx.DrawRectangle(pen, new Rectangle(fImageViewport.Left, fImageViewport.Top, fImageSize.Width - 1, fImageSize.Height - 1));
            }
            using (var pen = new Pen(Colors.Blue, 1.0f)) {
                gfx.DrawRectangle(pen, new Rectangle(fViewport.Left, fViewport.Top, fViewport.Width - 1, fViewport.Height - 1));
            }
            using (var brush = new SolidBrush(Colors.Fuchsia)) {
                Point center = fImageRect.Center;
                gfx.FillRectangle(brush, new Rectangle(center.X - 3, center.Y - 3, 6, 6));
            }
            #endif
        }

        protected virtual void OnPaint(PaintEventArgs e)
        {
        }

        private void UpdateProperties()
        {
            //if (fViewport.IsEmpty) return;

            fHasHScroll = (fViewport.Width < fImageSize.Width);
            fHasVScroll = (fViewport.Height < fImageSize.Height);

            int destX, destY;

            if (fHasHScroll) {
                destX = 0;
                fMouseOffsetX = fViewport.Left;
            } else {
                if (fCenteredImage) {
                    destX = (fViewport.Width - fImageSize.Width) / 2;
                    fMouseOffsetX = -destX;
                } else {
                    destX = 0;
                    fMouseOffsetX = 0;
                }
            }

            if (fHasVScroll) {
                destY = 0;
                fMouseOffsetY = fViewport.Top;
            } else {
                if (fCenteredImage) {
                    destY = (fViewport.Height - fImageSize.Height) / 2;
                    fMouseOffsetY = -destY;
                } else {
                    destY = 0;
                    fMouseOffsetY = 0;
                }
            }

            fImageRect = new Rectangle(destX, destY, fImageSize.Width, fImageSize.Height);

            int width = Math.Min(fImageSize.Width, fViewport.Width);
            int height = Math.Min(fImageSize.Height, fViewport.Height);
            fImageViewport = new Rectangle(destX, destY, width, height);
        }

        // unsupported in Wpf and maybe in other platforms (exclude WinForms), don't use
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

            e.Handled = true;
            base.OnMouseDown(e);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            int delta = -(int)(e.Delta.Height * 120.0f);

            if (Keys.None == e.Modifiers) {
                AdjustScroll(0, delta);
            } else if (Keys.Shift == e.Modifiers) {
                AdjustScroll(delta, 0);
            }

            e.Handled = true;
            base.OnMouseWheel(e);
        }

        protected override void OnShown(EventArgs e)
        {
            if (Loaded) {
                try {
                    fViewport = VisibleRect;
                    UpdateProperties();
                } catch (Exception ex) {
                    // FIXME: works in MacOS and doesn't work in Wpf
                }
            }

            base.OnShown(e);
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            if (Loaded) {
                fViewport = VisibleRect;
                UpdateProperties();
            }
            base.OnSizeChanged(e);
        }

        protected override void OnScroll(ScrollEventArgs e)
        {
            if (Loaded) {
                fViewport = VisibleRect;
                UpdateProperties();
                fCanvas.Invalidate();
            }
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
        /// Sets the sizes of canvas.
        /// </summary>
        /// <param name="imageSize">The size of canvas.</param>
        /// <param name="noRedraw">Flag of the need to redraw.</param>
        protected void SetImageSize(ExtSize imageSize, bool noRedraw = false)
        {
            if (!imageSize.IsEmpty) {
                fImageSize = new Size(imageSize.Width, imageSize.Height);

                Size clientSize = fViewport.Size;
                int canvWidth = Math.Max(imageSize.Width, clientSize.Width);
                int canvHeight = Math.Max(imageSize.Height, clientSize.Height);
                fCanvas.Size = new Size(canvWidth, canvHeight);

                base.UpdateScrollSizes();
                UpdateProperties();
            }

            if (!noRedraw) Invalidate();
        }

        protected Point GetImageRelativeLocation(PointF mpt)
        {
            return new Point((int)mpt.X + fMouseOffsetX, (int)mpt.Y + fMouseOffsetY);
        }

        protected PointF GetControlRelativeLocation(PointF mpt)
        {
            return new PointF(mpt.X - fMouseOffsetX, mpt.Y - fMouseOffsetY);
        }

        protected void InvalidateContent()
        {
            fCanvas.Invalidate();
        }
    }
}
