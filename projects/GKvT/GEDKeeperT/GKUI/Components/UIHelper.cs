/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Text;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Options;
using Terminal.Gui;

namespace GKUI.Components
{
    /// <summary>
    /// Static functions only for UI implementation.
    /// </summary>
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

        public static T GetSelectedTag<T>(this ComboBox comboBox)
        {
            var dataList = comboBox.Source.ToList();

            int selectedIndex = comboBox.SelectedItem;
            var comboItem = (selectedIndex >= 0 && selectedIndex < dataList.Count) ? dataList[selectedIndex] as ComboItem<T> : null;
            return (comboItem != null) ? comboItem.Tag : default;
        }

        public static void SetSelectedTag<T>(this ComboBox comboBox, T tagValue, bool allowDefault = true)
        {
            var dataList = comboBox.Source.ToList();

            for (int i = 0; i < dataList.Count; i++) {
                object item = dataList[i];
                var comboItem = item as ComboItem<T>;

                if (comboItem != null && object.Equals(comboItem.Tag, tagValue)) {
                    comboBox.SelectedItem = i;
                    return;
                }
            }

            if (allowDefault) {
                comboBox.SelectedItem = 0;
            }
        }

        public static MenuItem AddToolStripItem(MenuBarItem contextMenu, string text, object tag, EventHandler clickHandler)
        {
            var items = contextMenu.Children;
            var len = items.Length;
            Array.Resize(ref items, len + 1);

            var tsItem = new MenuItem(text, "", clickHandler);
            tsItem.Tag = tag;
            items[len] = tsItem;
            contextMenu.Children = items;
            return tsItem;
        }

        public static T GetMenuItemTag<T>(MenuBarItem contextMenu, object sender)
        {
            foreach (var tsItem in contextMenu.Children) {
                tsItem.Checked = false;
            }
            var senderItem = (MenuItem)sender;
            ((MenuItem)sender).Checked = true;
            return (T)senderItem.Tag;
        }

        public static void SetMenuItemTag<T>(MenuBarItem contextMenu, T value)
        {
            foreach (var tsItem in contextMenu.Children) {
                T itemTag = (T)tsItem.Tag;
                if (Equals(itemTag, value)) {
                    tsItem.PerformAction();
                    break;
                }
            }
        }

        public static GKListView CreateRecordsView(View parent, BaseContext baseContext, GDMRecordType recType, bool simpleList)
        {
            if (parent == null)
                throw new ArgumentNullException(nameof(parent));

            if (baseContext == null)
                throw new ArgumentNullException(nameof(baseContext));

            GKListView recView = new GKListView();
            recView.Location = new Point(0, 0);
            recView.Height = Dim.Fill();
            recView.Width = Dim.Fill();
            recView.ListMan = RecordsListModel<GDMRecord>.Create(baseContext, recType, simpleList);
            parent.Add(recView);

            return recView;
        }

        public static GKListView CreateListView(View parent)
        {
            if (parent == null)
                throw new ArgumentNullException(nameof(parent));

            GKListView listView = new GKListView();
            listView.Width = Dim.Fill();
            listView.Height = Dim.Fill();
            parent.Add(listView);

            return listView;
        }

        public static void ProcessName(object sender)
        {
            if (sender is TextField tb && GlobalOptions.Instance.FirstCapitalLetterInNames) {
                tb.Text = GKUtils.UniformName(tb.Text.ToString());
            }

            if (sender is ComboBox cmb && GlobalOptions.Instance.FirstCapitalLetterInNames) {
                cmb.Text = GKUtils.UniformName(cmb.Text.ToString());
            }
        }

        public static string[] Convert(string text)
        {
            var strList = new StringList(text);
            return strList.ToArray();
        }

        public static string Convert(string[] lines)
        {
            StringBuilder strBuilder = new StringBuilder();
            foreach (var line in lines) {
                if (strBuilder.Length > 0) {
                    strBuilder.Append(Environment.NewLine);
                }
                strBuilder.Append(line);
            }
            return strBuilder.ToString();
        }

        public static void SetupComboBox(ComboBox comboBox)
        {
            comboBox.MaxDropDownItems = 10;
            comboBox.HideDropdownListOnClick = true;
            comboBox.SearchMode = false;
            comboBox.DropDownBorderStyle = BorderStyle.Single;
        }
    }
}
