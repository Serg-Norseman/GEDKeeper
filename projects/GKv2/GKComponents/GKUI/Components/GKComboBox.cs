/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Drawing;
using System.Windows.Forms;
using GKCore.Design.Controls;
using GKUI.Platform.Handlers;

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
