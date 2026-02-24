/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using Terminal.Gui;

namespace GKUI.Components
{
    public static class UIHelper
    {
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
    }
}
