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
using Terminal.Gui;
using Attribute = Terminal.Gui.Attribute;

namespace GKUI.Components
{
    public class ScrollablePanel : ScrollView, IScrollableContainer
    {
        private class GraphContent : View
        {
            private ScrollablePanel fPanel;

            public GraphContent(ScrollablePanel panel)
            {
                fPanel = panel;
            }

            public override void Redraw(Rect region)
            {
                base.Redraw(region);
                Driver.SetAttribute(ColorScheme.Normal);
                fPanel.OnPaint(this);
            }
        }

        private readonly GraphContent fCanvas;

        public const int SmallChange = 1;
        public const int LargeChange = 10;

        private bool fCenteredImage;
        private bool fHasHScroll;
        private bool fHasVScroll;
        private Rect fImageRect;
        private Size fImageSize;
        private ExtRect fImageViewport;
        private int fMouseOffsetX, fMouseOffsetY;
        private Attribute? fTextColor;
        private ExtRect fViewport;

        public Rect CanvasRectangle
        {
            get { return new Rect(0, 0, ContentSize.Width, ContentSize.Height); }
        }

        protected bool CenteredImage
        {
            get { return fCenteredImage; }
            set {
                fCenteredImage = value;
                UpdateProperties();
            }
        }

        public Rect ClientRectangle
        {
            get { return Bounds; }
        }

        protected Rect ImageRect
        {
            get { return fImageRect; }
        }

        public ExtRect ImageViewport
        {
            get { return fImageViewport; }
        }

        public Point MouseOffset
        {
            get { return new Point(fMouseOffsetX, fMouseOffsetY); }
        }

        public bool HScroll
        {
            get { return fHasHScroll; }
        }

        public bool VScroll
        {
            get { return fHasVScroll; }
        }

        public Attribute TextColor
        {
            get { return fTextColor ?? ColorScheme.Normal; }
            set { fTextColor = value; }
        }

        public ExtRect Viewport
        {
            get { return fViewport; }
        }

        public bool VirtualCanvas
        {
            get { return false; }
        }

        public ScrollablePanel()
        {
            //Border = new Border() { BorderStyle = BorderStyle.Single };

            ShowVerticalScrollIndicator = true;
            ShowHorizontalScrollIndicator = true;
            AutoHideScrollBars = false;
            CanFocus = true;

            fCanvas = new GraphContent(this);
            fCanvas.X = 0;
            fCanvas.Y = 0;
            fCanvas.CanFocus = false;
            Add(fCanvas);

            //UIHelper.AddPrivEvents(this, "vertical", "ChangedPosition", OnScroll);
            //UIHelper.AddPrivEvents(this, "horizontal", "ChangedPosition", OnScroll);*/
        }

        protected virtual void OnPaint(View content)
        {
        }

        /*private void OnScroll()
        {
            SysUtils.DoNotInline(this);
            UpdateProperties();
        }*/

        private void UpdateProperties()
        {
            int canvWidth = Math.Max(fImageSize.Width, Bounds.Width);
            int canvHeight = Math.Max(fImageSize.Height, Bounds.Height);

            if (ContentSize.Width != canvWidth || ContentSize.Height != canvHeight) {
                ContentSize = new Size(canvWidth, canvHeight);
                fCanvas.Width = canvWidth;
                fCanvas.Height = canvHeight;
            }

            var clientRect = base.Bounds;
            var scrollPos = base.ContentOffset;

            fViewport = new ExtRect(scrollPos.X, scrollPos.Y, clientRect.Width, clientRect.Height);

            fHasHScroll = (fViewport.Width < fImageSize.Width);
            fHasVScroll = (fViewport.Height < fImageSize.Height);

            int destX = 0, destY = 0;

            if (fHasHScroll) {
                fMouseOffsetX = fViewport.Left;
            } else if (fCenteredImage) {
                destX = (fViewport.Width - fImageSize.Width) / 2;
                fMouseOffsetX = -destX;
            }

            if (fHasVScroll) {
                fMouseOffsetY = fViewport.Top;
            } else if (fCenteredImage) {
                destY = (fViewport.Height - fImageSize.Height) / 2;
                fMouseOffsetY = -destY;
            }

            fImageRect = new Rect(destX, destY, fImageSize.Width, fImageSize.Height);
            int width = Math.Min(fImageSize.Width, fViewport.Width);
            int height = Math.Min(fImageSize.Height, fViewport.Height);
            fImageViewport = ExtRect.CreateBounds(destX, destY, width, height);
        }

        public override bool OnEnter(View nextFocused)
        {
            UpdateProperties();
            return base.OnEnter(nextFocused);
        }

        public override bool OnKeyDown(KeyEvent keyEvent)
        {
            bool result = false;

            if (Enabled && HasFocus) {
                var key = keyEvent.Key;
                bool hasCtrl = false;
                if ((key & Key.CtrlMask) == Key.CtrlMask) {
                    hasCtrl = true;
                    key = key & ~Key.CtrlMask;
                }

                switch (key) {
                    case Key.CursorUp:
                        result = ScrollUp(1);
                        break;
                    case Key.CursorDown:
                        result = ScrollDown(1);
                        break;
                    case Key.CursorLeft:
                        result = ScrollLeft(1);
                        break;
                    case Key.CursorRight:
                        result = ScrollRight(1);
                        break;

                    case Key.PageUp:
                        result = ScrollUp(Bounds.Height);
                        break;
                    case Key.PageDown:
                        result = ScrollDown(Bounds.Height);
                        break;

                    case Key.Home:
                        result = ScrollLeft(ContentSize.Width);
                        if (hasCtrl) {
                            result = result && ScrollUp(ContentSize.Height);
                        }
                        break;

                    case Key.End:
                        result = ScrollRight(ContentSize.Width);
                        if (hasCtrl) {
                            result = result && ScrollDown(ContentSize.Height);
                        }
                        break;
                }
            }

            return result;
        }

        public override bool MouseEvent(MouseEvent me)
        {
            if (me.Flags == MouseFlags.Button1Pressed && !HasFocus) {
                SetFocus();
                return true;
            }

            if (me.Flags.HasFlag(MouseFlags.WheeledDown)) {
                if (me.Flags.HasFlag(MouseFlags.ButtonShift)) {
                    ScrollRight(1);
                } else ScrollDown(1);
                return true;
            }

            if (me.Flags.HasFlag(MouseFlags.WheeledUp)) {
                if (me.Flags.HasFlag(MouseFlags.ButtonShift)) {
                    ScrollLeft(1);
                } else ScrollUp(1);
                return true;
            }

            return base.MouseEvent(me);
        }

        /// <summary>
        /// Updates the scroll position.
        /// </summary>
        /// <param name="posX">The X position.</param>
        /// <param name="posY">The Y position.</param>
        protected void UpdateScrollPosition(int posX, int posY)
        {
            ContentOffset = new Point(posX, posY);
            UpdateProperties();
            //InvalidateContent();
        }

        /// <summary>
        /// Adjusts the scroll.
        /// </summary>
        /// <param name="dx">The X shift.</param>
        /// <param name="dy">The Y shift.</param>
        protected void AdjustScroll(int dx, int dy)
        {
            var scrollPos = ContentOffset;
            UpdateScrollPosition(scrollPos.X + dx, scrollPos.Y + dy);
        }

        /// <summary>
        /// Sets the sizes of canvas.
        /// </summary>
        /// <param name="imageSize">The size of canvas.</param>
        /// <param name="noRedraw">Flag of the need to redraw.</param>
        protected void SetImageSize(ExtSize imageSize, bool noRedraw = false)
        {
            if (!imageSize.IsEmpty && (fImageSize.Width != imageSize.Width || fImageSize.Height != imageSize.Height)) {
                fImageSize = new Size(imageSize.Width, imageSize.Height);

                UpdateProperties();
            }

            if (!noRedraw) InvalidateContent();
        }

        protected Point GetImageRelativeLocation(Point mpt, bool buttons)
        {
            return new Point(mpt.X + fMouseOffsetX, mpt.Y + fMouseOffsetY);
        }

        protected void InvalidateContent()
        {
            fCanvas.SetNeedsDisplay();
        }
    }
}
