/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using GKCore.Interfaces;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class ScrollablePanel : Panel, IScrollableContainer
    {
        private bool fCenteredImage;
        private Size fImageSize;
        private ExtRect fImageViewport;
        //private ExtRect fViewport;

        protected bool CenteredImage
        {
            get { return fCenteredImage; }
            set { fCenteredImage = value; }
        }

        public ExtRect ImageViewport
        {
            get { return fImageViewport; }
        }

        public ExtRect Viewport
        {
            get {
                var scrollPos = AutoScrollPosition;
                var clientSize = ClientSize;
                return ExtRect.CreateBounds(Math.Abs(scrollPos.X), Math.Abs(scrollPos.Y), clientSize.Width, clientSize.Height);
                //return fViewport;
            }
        }

        public bool VirtualCanvas
        {
            get { return true; }
        }


        public ScrollablePanel()
        {
            AutoScroll = true;
            ResizeRedraw = true;
        }

        protected Rectangle GetClientRect(bool includePadding)
        {
            int left = 0;
            int top = 0;
            int width = ClientSize.Width;
            int height = ClientSize.Height;

            if (includePadding) {
                left += Padding.Left;
                top += Padding.Top;
                width -= Padding.Horizontal;
                height -= Padding.Vertical;
            }

            return new Rectangle(left, top, width, height);
        }

        private void UpdateProperties()
        {
            //if (fViewport.IsEmpty) return;

            var viewport = this.Viewport;
            var fHasHScroll = (viewport.Width < fImageSize.Width);
            var fHasVScroll = (viewport.Height < fImageSize.Height);

            int destX, destY;

            if (fHasHScroll) {
                destX = 0;
                //fMouseOffsetX = fViewport.Left;
            } else {
                if (fCenteredImage) {
                    destX = (viewport.Width - fImageSize.Width) / 2;
                    //fMouseOffsetX = -destX;
                } else {
                    destX = 0;
                    //fMouseOffsetX = 0;
                }
            }

            if (fHasVScroll) {
                destY = 0;
                //fMouseOffsetY = fViewport.Top;
            } else {
                if (fCenteredImage) {
                    destY = (viewport.Height - fImageSize.Height) / 2;
                    //fMouseOffsetY = -destY;
                } else {
                    destY = 0;
                    //fMouseOffsetY = 0;
                }
            }

            //fImageRect = new Rectangle(destX, destY, fImageSize.Width, fImageSize.Height);

            int width = Math.Min(fImageSize.Width, viewport.Width);
            int height = Math.Min(fImageSize.Height, viewport.Height);
            fImageViewport = ExtRect.CreateBounds(destX, destY, width, height);
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if (!Focused) {
                Focus();
            }

            base.OnMouseDown(e);
        }

        protected override void OnScroll(ScrollEventArgs e)
        {
            if (e.Type == ScrollEventType.ThumbTrack) {
                switch (e.ScrollOrientation) {
                    case ScrollOrientation.HorizontalScroll:
                        AutoScrollPosition = new Point(e.NewValue, -AutoScrollPosition.Y);
                        break;

                    case ScrollOrientation.VerticalScroll:
                        AutoScrollPosition = new Point(-AutoScrollPosition.X, e.NewValue);
                        break;
                }
            }

            UpdateProperties();

            base.OnScroll(e);
        }

        /// <summary>
        /// Updates the scroll position.
        /// </summary>
        /// <param name="posX">The X position.</param>
        /// <param name="posY">The Y position.</param>
        protected void UpdateScrollPosition(int posX, int posY)
        {
            AutoScrollPosition = new Point(posX, posY);
        }

        /// <summary>
        /// Adjusts the scroll.
        /// </summary>
        /// <param name="dx">The X shift.</param>
        /// <param name="dy">The Y shift.</param>
        protected void AdjustScroll(int dx, int dy)
        {
            UpdateScrollPosition(HorizontalScroll.Value + dx, VerticalScroll.Value + dy);
        }

        /// <summary>
        /// Sets the sizes of canvas.
        /// </summary>
        /// <param name="imageSize">The size of canvas.</param>
        /// <param name="noRedraw">Flag of the need to redraw.</param>
        protected void SetImageSize(ExtSize imageSize, bool noRedraw = false)
        {
            if (AutoScroll && !imageSize.IsEmpty) {
                fImageSize = new Size(imageSize.Width, imageSize.Height);

                AutoScrollMinSize = new Size(imageSize.Width + Padding.Horizontal, imageSize.Height + Padding.Vertical);

                UpdateProperties();
            }

            if (!noRedraw) Invalidate();
        }

        protected Point GetImageRelativeLocation(PointF mpt)
        {
            return new Point((int)mpt.X + Math.Abs(AutoScrollPosition.X), (int)mpt.Y + Math.Abs(AutoScrollPosition.Y));
        }
    }
}
