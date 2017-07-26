/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using System.Drawing.Drawing2D;
using System.Windows.Forms;

namespace GKUI.Components
{
    public class GKTabControl : TabControl
    {
        private const int TAB_PADDING = 5;

        public GKTabControl() : base()
        {
            DrawMode = TabDrawMode.OwnerDrawFixed;
            Padding = new Point(20, 4);
            SizeMode = TabSizeMode.Normal;
        }

        protected override void OnDrawItem(DrawItemEventArgs e)
        {
            var gfx = e.Graphics;
            TabPage tabPage = TabPages[e.Index];
            bool isSelected = (e.State == DrawItemState.Selected);

            int iconX = e.Bounds.X, iconY = 0;

            // I used this as a rectangle because otherwise there is nasty looking line below each tab.
            Rectangle innerRec = new Rectangle(e.Bounds.X, e.Bounds.Y, e.Bounds.Width, e.Bounds.Height + 2);

            // Draw the border background.
            if (isSelected)
            {
                // Selected tab
                using (var brush = new SolidBrush(Color.White))
                    gfx.FillRectangle(brush, innerRec);
            }
            else if (tabPage != null && e.State == DrawItemState.HotLight)
            {
                // When hovering over not selected tab.
                using (var brush = new LinearGradientBrush(innerRec, Color.White, Color.LightBlue, LinearGradientMode.Vertical))
                    gfx.FillRectangle(brush, innerRec);
            }
            else
            {
                // Non selected tab.
                using (var brush = new LinearGradientBrush(innerRec, Color.White, Color.LightGray, LinearGradientMode.Vertical))
                    gfx.FillRectangle(brush, innerRec);
            }

            if (tabPage != null)
            {
                string tabText = tabPage.Text;

                // Draw the tab icon if exists.
                if (ImageList != null && (tabPage.ImageIndex > -1 || !string.IsNullOrEmpty(tabPage.ImageKey)))
                {
                    Image img;

                    if (tabPage.ImageIndex > -1)
                        img = ImageList.Images[tabPage.ImageIndex];
                    else
                        img = ImageList.Images[tabPage.ImageKey];

                    if (img != null)
                    {
                        iconX = innerRec.X + TAB_PADDING;
                        iconY = (innerRec.Height - img.Height) / 2;

                        gfx.DrawImageUnscaled(img, iconX, iconY + innerRec.Y);

                        iconX += img.Width;
                    }
                }

                var closeRect = GetCloseButtonRect(innerRec);

                // Draw the close button.
                //if (tab.HasCloseButton)
                {
                    Image closeImg = ExtResources.iBtnClose;

                    if (closeImg != null) {
                        gfx.DrawImageUnscaled(closeImg, closeRect.Left, closeRect.Top + innerRec.Y);
                    } else {
                        using (var xFont = new Font(e.Font, FontStyle.Bold)) {
                            Font font = (isSelected) ? xFont : e.Font;
                            gfx.DrawString("x", xFont, Brushes.Black, closeRect.Left, closeRect.Top);
                        }
                    }
                }

                // Draw the text. If the text is too long then cut off the end and add '...'
                // To avoid this behaviour, set the TabControl.ItemSize to larger value.

                var tabFont = isSelected ? new Font(e.Font, FontStyle.Bold) : e.Font;
                var tabTextSize = gfx.MeasureString(tabText, tabFont);

                var textX = iconX + TAB_PADDING;
                var textY = (innerRec.Height - tabTextSize.Height + innerRec.Y) / 2;

                // Calculate if the text fits as is. If not then trim it.
                if (textX + tabTextSize.Width > closeRect.Left - TAB_PADDING)
                    tabText = TrimTextToFit(gfx, tabText.Substring(0, tabText.Length - 1), tabFont, closeRect.Left - TAB_PADDING - textX);

                gfx.DrawString(tabText, tabFont, Brushes.Black, textX, textY + innerRec.Y);
            }
        }

        private static string TrimTextToFit(Graphics g, string text, Font font, int width)
        {
            if (string.IsNullOrEmpty(text))
                return text;
            
            string txtToFit = text + "...";
            if (g.MeasureString(txtToFit, font).Width <= width)
                return txtToFit;
            else
                // Remove the last character and try again.
                return TrimTextToFit(g, text.Substring(0, text.Length - 1), font, width);
        }

        private Rectangle GetCloseButtonRect(Rectangle tabRect)
        {
            Image closeImg = ExtResources.iBtnClose;
            int closeX = tabRect.Right - TAB_PADDING - closeImg.Width;
            int closeY = (tabRect.Height - closeImg.Height + tabRect.Y) / 2;
            var closeButton = new Rectangle(closeX, closeY, closeImg.Width, closeImg.Height);
            return closeButton;
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (e.Button == MouseButtons.Left) {
                Rectangle closeButton = GetCloseButtonRect(GetTabRect(SelectedIndex));
                if (closeButton.Contains(e.Location)) {
                    TabPages.Remove(SelectedTab);
                }
            }
        }
    }
}
