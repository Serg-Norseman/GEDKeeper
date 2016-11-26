/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

namespace GKCommon.Controls
{
    public class GKScrollableControl : Panel
    {
        //private bool fValidEvent;

        public GKScrollableControl()
        {
            base.AutoScroll = true;
            base.ResizeRedraw = true;

            #if __MonoCS__
            ScrollBar obj = SysUtils.GetFieldValue(this, "hscrollbar") as ScrollBar;
            if (obj != null) {
                SysUtils.RemoveControlStdEventHandlers(obj, "ScrollEvent");
                obj.Scroll += new ScrollEventHandler(HandleHScrollEvent);
            }

            obj = SysUtils.GetFieldValue(this, "vscrollbar") as ScrollBar;
            if (obj != null) {
                SysUtils.RemoveControlStdEventHandlers(obj, "ScrollEvent");
                obj.Scroll += new ScrollEventHandler(HandleVScrollEvent);
            }
            #else
            //this.fValidEvent = true;
            #endif
        }

        #if __MonoCS__
        private void HandleHScrollEvent (object sender, ScrollEventArgs args)
        {
            //this.fValidEvent = true;
            ScrollEventArgs newArgs = new ScrollEventArgs(args.Type, args.OldValue, args.NewValue, ScrollOrientation.HorizontalScroll);
            this.OnScroll(newArgs);
            //this.fValidEvent = false;
        }

        private void HandleVScrollEvent (object sender, ScrollEventArgs args)
        {
            //this.fValidEvent = true;
            ScrollEventArgs newArgs = new ScrollEventArgs(args.Type, args.OldValue, args.NewValue, ScrollOrientation.VerticalScroll);
            this.OnScroll(newArgs);
            //this.fValidEvent = false;
        }
        #endif

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (!this.Focused)
                this.Focus();
        }

        /// <summary>
        ///   Raises the <see cref="System.Windows.Forms.ScrollableControl.Scroll" /> event.
        /// </summary>
        /// <param name="e">
        ///   A <see cref="T:System.Windows.Forms.ScrollEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnScroll(ScrollEventArgs se)
        {
            this.Invalidate();

            if (se.Type != ScrollEventType.EndScroll /*&& this.fValidEvent*/)
            {
                switch (se.ScrollOrientation)
                {
                    case ScrollOrientation.HorizontalScroll:
                        this.ScrollByOffset(new Size(se.NewValue + this.AutoScrollPosition.X, 0));
                        break;

                    case ScrollOrientation.VerticalScroll:
                        this.ScrollByOffset(new Size(0, se.NewValue + this.AutoScrollPosition.Y));
                        break;
                }
            }

            base.OnScroll(se);
        }

        private void ScrollByOffset(Size offset)
        {
            if (!offset.IsEmpty)
            {
                this.SuspendLayout();
                foreach (Control child in Controls) {
                    child.Location -= offset;
                }

                this.AutoScrollPosition = new Point(-(AutoScrollPosition.X - offset.Width), -(AutoScrollPosition.Y - offset.Height));

                this.ResumeLayout();
                this.Invalidate();
                //this.Update();
                //this.Refresh();
            }
        }

        /// <summary>
        /// Adjusts the scroll.
        /// </summary>
        /// <param name="x">The x.</param>
        /// <param name="y">The y.</param>
        protected void AdjustScroll(int x, int y)
        {
            Point scrollPosition = new Point(this.HorizontalScroll.Value + x, this.VerticalScroll.Value + y);
            this.UpdateScrollPosition(scrollPosition);
        }

        /// <summary>
        /// Updates the scroll position.
        /// </summary>
        /// <param name="position">The position.</param>
        protected void UpdateScrollPosition(Point position)
        {
            this.AutoScrollPosition = position;
            this.Invalidate();
            this.OnScroll(new ScrollEventArgs(ScrollEventType.EndScroll, 0));
        }

        protected void AdjustViewPort(Size imageSize, bool noRedraw = false)
        {
            if (this.AutoScroll && !imageSize.IsEmpty) {
                this.AutoScrollMinSize = new Size(imageSize.Width + this.Padding.Horizontal, imageSize.Height + this.Padding.Vertical);
            }

            if (!noRedraw) this.Invalidate();
        }
    }
}
