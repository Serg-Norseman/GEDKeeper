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
using System.ComponentModel;
using System.Drawing;
using System.Reflection;
using System.Windows.Forms;

using GKCommon;

namespace GKUI.Components
{
    public class ScrollablePanel : Panel
    {
        //private bool fValidEvent;

        public ScrollablePanel()
        {
            AutoScroll = true;
            ResizeRedraw = true;

            #if __MonoCS__
            ScrollBar obj = SysUtils.GetFieldValue(this, "hscrollbar") as ScrollBar;
            if (obj != null) {
                RemoveControlStdEventHandlers(obj, "ScrollEvent");
                obj.Scroll += new ScrollEventHandler(HandleHScrollEvent);
            }

            obj = SysUtils.GetFieldValue(this, "vscrollbar") as ScrollBar;
            if (obj != null) {
                RemoveControlStdEventHandlers(obj, "ScrollEvent");
                obj.Scroll += new ScrollEventHandler(HandleVScrollEvent);
            }
            #else
            //fValidEvent = true;
            #endif
        }

        private static void RemoveControlStdEventHandlers(Control ctl, string privateEventObj)
        {
            if (ctl == null)
                throw new ArgumentNullException("ctl");

            FieldInfo f1 = SysUtils.FindFieldInfo(ctl.GetType(), privateEventObj);
            if (f1 == null) return;

            object obj = f1.GetValue(ctl);

            PropertyInfo pi = ctl.GetType().GetProperty("Events", BindingFlags.NonPublic | BindingFlags.Instance);
            if (pi == null) return;

            EventHandlerList list = (EventHandlerList)pi.GetValue(ctl, null);
            list.RemoveHandler(obj, list[obj]);
        }

        #if __MonoCS__

        private void HandleHScrollEvent(object sender, ScrollEventArgs args)
        {
            //fValidEvent = true;
            ScrollEventArgs newArgs = new ScrollEventArgs(args.Type, args.OldValue, args.NewValue, ScrollOrientation.HorizontalScroll);
            OnScroll(newArgs);
            //fValidEvent = false;
        }

        private void HandleVScrollEvent(object sender, ScrollEventArgs args)
        {
            //fValidEvent = true;
            ScrollEventArgs newArgs = new ScrollEventArgs(args.Type, args.OldValue, args.NewValue, ScrollOrientation.VerticalScroll);
            OnScroll(newArgs);
            //fValidEvent = false;
        }

        #endif

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if (!Focused)
                Focus();

            base.OnMouseDown(e);
        }

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.ScrollableControl.Scroll" /> event.
        /// </summary>
        /// <param name="se">
        ///   A <see cref="T:System.Windows.Forms.ScrollEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnScroll(ScrollEventArgs se)
        {
            Invalidate();

            if (se.Type != ScrollEventType.EndScroll /*&& fValidEvent*/)
            {
                switch (se.ScrollOrientation)
                {
                    case ScrollOrientation.HorizontalScroll:
                        ScrollByOffset(new Size(se.NewValue + AutoScrollPosition.X, 0));
                        break;

                    case ScrollOrientation.VerticalScroll:
                        ScrollByOffset(new Size(0, se.NewValue + AutoScrollPosition.Y));
                        break;
                }
            }

            base.OnScroll(se);
        }

        private void ScrollByOffset(Size offset)
        {
            if (offset.IsEmpty) return;

            SuspendLayout();
            foreach (Control child in Controls) {
                child.Location -= offset;
            }

            AutoScrollPosition = new Point(-(AutoScrollPosition.X - offset.Width), -(AutoScrollPosition.Y - offset.Height));

            ResumeLayout();
            Invalidate();
            //Update();
            //Refresh();
        }

        /// <summary>
        /// Adjusts the scroll.
        /// </summary>
        /// <param name="x">The x.</param>
        /// <param name="y">The y.</param>
        protected void AdjustScroll(int x, int y)
        {
            Point scrollPosition = new Point(HorizontalScroll.Value + x, VerticalScroll.Value + y);
            UpdateScrollPosition(scrollPosition);
        }

        /// <summary>
        /// Updates the scroll position.
        /// </summary>
        /// <param name="position">The position.</param>
        protected void UpdateScrollPosition(Point position)
        {
            AutoScrollPosition = position;
            Invalidate();
            OnScroll(new ScrollEventArgs(ScrollEventType.EndScroll, 0));
        }

        protected void AdjustViewPort(Size imageSize, bool noRedraw = false)
        {
            if (AutoScroll && !imageSize.IsEmpty) {
                AutoScrollMinSize = new Size(imageSize.Width + Padding.Horizontal, imageSize.Height + Padding.Vertical);
            }

            if (!noRedraw) Invalidate();
        }
    }
}
