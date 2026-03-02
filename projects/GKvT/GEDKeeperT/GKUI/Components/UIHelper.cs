/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using Terminal.Gui;

namespace GKUI.Components
{
    public static class UIHelper
    {
        public static Rect Rt2Rt(ExtRect ert)
        {
            return new Rect(ert.Left, ert.Top, ert.GetWidth(), ert.GetHeight());
        }

        public static ExtRect Rt2Rt(Rect ert)
        {
            return ExtRect.CreateBounds(ert.Left, ert.Top, ert.Width, ert.Height);
        }

        public static ToolStripMenuItem AddToolStripItem(ToolStripDropDownButton contextMenu, string text, object tag, EventHandler clickHandler)
        {
            var items = contextMenu.Children;
            var len = items.Length;
            Array.Resize(ref items, len + 1);

            var tsItem = new ToolStripMenuItem(text, "", clickHandler);
            tsItem.Tag = tag;
            items[len] = tsItem;
            contextMenu.Children = items;
            return tsItem;
        }

        public static T GetMenuItemTag<T>(ToolStripDropDownButton contextMenu, object sender)
        {
            foreach (var tsItem in contextMenu.Children) {
                tsItem.Checked = false;
            }
            var senderItem = (MenuItem)sender;
            ((MenuItem)sender).Checked = true;
            return (T)senderItem.Tag;
        }

        public static void SetMenuItemTag<T>(ToolStripDropDownButton contextMenu, T value)
        {
            foreach (var tsItem in contextMenu.Children) {
                T itemTag = (T)tsItem.Tag;
                if (Equals(itemTag, value)) {
                    tsItem.PerformAction();
                    break;
                }
            }
        }
    }
}
