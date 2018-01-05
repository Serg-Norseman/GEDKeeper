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

using System.Drawing;
using System.Windows.Forms;

using BSLib;

namespace GKUI.Components
{
    public class ScrollablePanel : Panel
    {
        public ScrollablePanel()
        {
            AutoScroll = true;
            ResizeRedraw = true;
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if (!Focused)
                Focus();

            base.OnMouseDown(e);
        }

        /// <summary>
        /// Raises the <see cref="System.Windows.Forms.ScrollableControl.Scroll" /> event.
        /// </summary>
        /// <param name="se">
        /// A <see cref="T:System.Windows.Forms.ScrollEventArgs" /> that contains the event data.
        /// </param>
        protected override void OnScroll(ScrollEventArgs se)
        {
            if (se.Type != ScrollEventType.EndScroll) {
                switch (se.ScrollOrientation)
                {
                    case ScrollOrientation.HorizontalScroll:
                        ScrollByOffset(se.NewValue + AutoScrollPosition.X, 0);
                        break;

                    case ScrollOrientation.VerticalScroll:
                        ScrollByOffset(0, se.NewValue + AutoScrollPosition.Y);
                        break;
                }
            } else {
                Invalidate();
            }

            base.OnScroll(se);
        }

        private void ScrollByOffset(int offsetX, int offsetY)
        {
            if (offsetX != 0 || offsetY != 0) {
                AutoScrollPosition = new Point(-(AutoScrollPosition.X - offsetX), -(AutoScrollPosition.Y - offsetY));
                #if !__MonoCS__
                Invalidate();
                #else
                Update();
                #endif
            }
        }

        /// <summary>
        /// Updates the scroll position.
        /// </summary>
        /// <param name="position">The position.</param>
        protected void UpdateScrollPosition(int posX, int posY)
        {
            AutoScrollPosition = new Point(posX, posY);
            Invalidate();
            //OnScroll(new ScrollEventArgs(ScrollEventType.EndScroll, 0));
        }

        /// <summary>
        /// Adjusts the scroll.
        /// </summary>
        /// <param name="x">The x.</param>
        /// <param name="y">The y.</param>
        protected void AdjustScroll(int x, int y)
        {
            UpdateScrollPosition(HorizontalScroll.Value + x, VerticalScroll.Value + y);
        }

        protected void AdjustViewport(ExtSize imageSize, bool noRedraw = false)
        {
            if (AutoScroll && !imageSize.IsEmpty) {
                AutoScrollMinSize = new Size(imageSize.Width + Padding.Horizontal, imageSize.Height + Padding.Vertical);
            }

            if (!noRedraw) Invalidate();
        }
    }
}
