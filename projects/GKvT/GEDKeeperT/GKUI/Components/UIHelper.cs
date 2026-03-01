/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
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

        public static ToolStripMenuItem AddToolStripItem(ToolStripDropDownButton contextMenu, string text, object tag, Action clickHandler)
        {
            var items = contextMenu.Children;
            var len = items.Length;
            Array.Resize(ref items, len + 1);

            var tsItem = new ToolStripMenuItem(text, "", clickHandler);
            items[len] = tsItem;
            contextMenu.Children = items;
            //tsItem.Tag = tag;
            return tsItem;
        }

        public static T GetMenuItemTag<T>(ToolStripDropDownButton contextMenu, object sender)
        {
            return default;
            /*foreach (ToolStripMenuItem tsItem in contextMenu.Items) {
                tsItem.Checked = false;
            }
            var senderItem = ((ToolStripMenuItem)sender);
            ((ToolStripMenuItem)sender).Checked = true;
            return (T)senderItem.Tag;*/
        }

        public static void SetMenuItemTag<T>(ToolStripDropDownButton contextMenu, T value)
        {
            /*foreach (ToolStripMenuItem tsItem in contextMenu.Items) {
                T itemTag = (T)tsItem.Tag;
                if (Equals(itemTag, value)) {
                    tsItem.PerformClick();
                    break;
                }
            }*/
        }

        public static void AddPrivEvents(object instance, string privateField, string eventName, Action eventHandler)
        {
            var instType = instance.GetType().BaseType.BaseType.BaseType;

            var privObj = instType.GetField(privateField, BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy | BindingFlags.GetField);
            if (privObj == null) return;

            var fieldVal = privObj.GetValue(instance);
            var fieldType = (fieldVal as ScrollBarView).GetType();
            var fieldInfo = fieldType.GetField(eventName, BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.GetField | BindingFlags.GetProperty);
            if (fieldInfo == null) return;

            MulticastDelegate eventDelegate = (MulticastDelegate)fieldInfo.GetValue(fieldVal);
            if (eventDelegate == null) return;

            fieldType.GetEvent(eventName).AddEventHandler(fieldVal, eventHandler);
        }

        // original is internal and inaccessible
        public static void ViewToScreen(View view, int col, int row, out int rcol, out int rrow)
        {
            var viewFrame = view.Frame;
            rcol = col + viewFrame.X;
            rrow = row + viewFrame.Y;

            var curContainer = view.SuperView;
            while (curContainer != null) {
                viewFrame = curContainer.Frame;
                rcol += viewFrame.X;
                rrow += viewFrame.Y;
                curContainer = curContainer.SuperView;
            }
        }
    }
}
