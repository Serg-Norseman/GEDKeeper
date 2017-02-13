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

namespace GKCommon.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public class GKComboBox : ComboBox
    {
        public GKComboBox()
        {
            DrawMode = DrawMode.OwnerDrawFixed;
            DropDownStyle = ComboBoxStyle.DropDownList;
        }

        protected override void OnDrawItem(DrawItemEventArgs e)
        {
            e.DrawBackground();
            e.DrawFocusRectangle();

            if (e.Index < 0 || e.Index >= Items.Count) return; 

            object item = Items[e.Index];
            DropDownItem ddItem = item as DropDownItem;
            if (ddItem != null) {
                int offset = e.Bounds.Left;

                if (ddItem.Image != null) {
                    e.Graphics.DrawImage(ddItem.Image, e.Bounds.Left, e.Bounds.Top);
                    offset += ddItem.Image.Width;
                }

                e.Graphics.DrawString(ddItem.Value, e.Font, new SolidBrush(e.ForeColor),
                                      offset, e.Bounds.Top + 2);
            } else {
                e.Graphics.DrawString(item.ToString(), e.Font, new SolidBrush(e.ForeColor),
                                      e.Bounds.Left, e.Bounds.Top + 2);
            }
            base.OnDrawItem(e);
        }
    }

    public class DropDownItem
    {
        private string fText;
        private Image fImage;

        public string Value
        {
            get { return fText; }
            set { fText = value; }
        }

        public Image Image
        {
            get { return fImage; }
            set { fImage = value; }
        }

        public DropDownItem()
        {
        }

        public DropDownItem(string text)
        {
            fText = text;
        }

        public DropDownItem(string text, Image image)
        {
            fText = text;
            fImage = image;
        }

        public override string ToString()
        {
            return fText;
        }
    }
}
