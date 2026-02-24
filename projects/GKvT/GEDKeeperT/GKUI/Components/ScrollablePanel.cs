/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore.Design;
using GKCore.Utilities;
using Terminal.Gui;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class ScrollablePanel : /*ScrollView, */ View, IScrollableContainer
    {
        /*private class GraphContent : View
        {
            private ScrollablePanel fPanel;

            public GraphContent(ScrollablePanel panel)
            {
                fPanel = panel;
            }

            public override void Redraw(Rect region)
            {
                //base.Redraw(region);
                //Driver.SetAttribute(ColorScheme.Normal);
                fPanel.OnPaint(this);
            }
        }


        private readonly GraphContent fCanvas;*/

        private bool fCenteredImage;
        private Size fImageSize;
        private ExtRect fImageViewport;
        private ExtRect fViewport;

        public bool AutoScroll { get; set; }

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
                /*var scrollPos = AutoScrollPosition;
                var clientSize = ClientSize;
                return ExtRect.CreateBounds(Math.Abs(scrollPos.X), Math.Abs(scrollPos.Y), clientSize.Width, clientSize.Height);*/
                var bounds = base.Bounds;
                return ExtRect.CreateBounds(0, 0, bounds.Width, bounds.Height);
                //return fViewport;
            }
        }

        public bool VirtualCanvas
        {
            get { return true; }
        }


        public ScrollablePanel()
        {
            Border = new Border() { BorderStyle = BorderStyle.Single };

            /*ShowVerticalScrollIndicator = true;
            ShowHorizontalScrollIndicator = true;
            AutoHideScrollBars = false;

            fCanvas = new GraphContent(this);
            Add(fCanvas);
            
            UIHelper.AddPrivEvents(this, "vertical", "ChangedPosition", OnScroll);
            UIHelper.AddPrivEvents(this, "horizontal", "ChangedPosition", OnScroll);*/

            AutoScroll = true;

            // ContentOffset View.OnDrawContent
        }

        private void OnScroll()
        {
            SysUtils.DoNotInline(this);
            UpdateProperties();
        }

        protected virtual void OnPaint(View content)
        {
        }

        public override void Redraw(Rect region)
        {
            base.Redraw(region);
            Driver.SetAttribute(ColorScheme.Normal);
            OnPaint(this);
        }

        /*protected Rectangle GetClientRect(bool includePadding)
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
        }*/

        private void UpdateProperties()
        {
            /*var clientRect = base.Bounds;
            var scrollPos = base.ContentOffset;

            //if (fViewport.IsEmpty) return;

            /*var viewport = this.Viewport;
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
            fImageViewport = ExtRect.CreateBounds(destX, destY, width, height);*/
        }

        /*protected override void OnMouseDown(MouseEventArgs e)
        {
            if (!Focused) {
                Focus();
            }

            base.OnMouseDown(e);
        }*/

        /*protected override void OnScroll(ScrollEventArgs e)
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
        }*/

        /// <summary>
        /// Updates the scroll position.
        /// </summary>
        /// <param name="posX">The X position.</param>
        /// <param name="posY">The Y position.</param>
        protected void UpdateScrollPosition(int posX, int posY)
        {
            //AutoScrollPosition = new Point(posX, posY);
        }

        /// <summary>
        /// Adjusts the scroll.
        /// </summary>
        /// <param name="dx">The X shift.</param>
        /// <param name="dy">The Y shift.</param>
        protected void AdjustScroll(int dx, int dy)
        {
            //UpdateScrollPosition(HorizontalScroll.Value + dx, VerticalScroll.Value + dy);
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

                //AutoScrollMinSize = new Size(imageSize.Width + Padding.Horizontal, imageSize.Height + Padding.Vertical);

                /*var clientSize = fViewport;
                int canvWidth = Math.Max(imageSize.Width, clientSize.Width);
                int canvHeight = Math.Max(imageSize.Height, clientSize.Height);

                ContentSize = new Size(canvWidth, canvHeight);
                fCanvas.Width = canvWidth;
                fCanvas.Height = canvHeight;*/

                UpdateProperties();
            }

            if (!noRedraw) Invalidate();
        }

        /*protected Point GetImageRelativeLocation(PointF mpt)
        {
            return new Point((int)mpt.X + Math.Abs(AutoScrollPosition.X), (int)mpt.Y + Math.Abs(AutoScrollPosition.Y));
        }*/

        public void Invalidate()
        {
            //fCanvas.Redraw(fCanvas.Bounds);

            base.Redraw(base.Bounds);
        }
    }
}
