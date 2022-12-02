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
using System.Windows.Forms;
using BSLib.Design.Handlers;
using BSLib.Design.MVP.Controls;

namespace GKUI.Components
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
            var ddItem = item as IComboItem;
            if (ddItem != null) {
                int offset = e.Bounds.Left;

                if (ddItem.Image != null) {
                    Image img = ((ImageHandler)ddItem.Image).Handle;
                    e.Graphics.DrawImage(img, e.Bounds.Left, e.Bounds.Top);
                    offset += ddItem.Image.Width;
                }

                e.Graphics.DrawString(ddItem.Text, e.Font, new SolidBrush(e.ForeColor),
                                      offset, e.Bounds.Top + 2);
            } else {
                e.Graphics.DrawString(item.ToString(), e.Font, new SolidBrush(e.ForeColor),
                                      e.Bounds.Left, e.Bounds.Top + 2);
            }
            base.OnDrawItem(e);
        }
    }
}
