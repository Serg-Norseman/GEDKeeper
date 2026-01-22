/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Utilities;
using Terminal.Gui;

namespace GKUI.Platform.Handlers
{
    public sealed class ComboBoxHandler : BaseControlHandler<ComboBox, ComboBoxHandler>, IComboBox
    {
        private readonly ExtObservableList<IComboItem> fItems;

        public ComboBoxHandler(ComboBox control) : base(control)
        {
            fItems = new ExtObservableList<IComboItem>();
            control.SetSource(fItems);
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set { Control.ReadOnly = value; }
        }

        public int SelectedIndex
        {
            get { return Control.SelectedItem; }
            set { Control.SelectedItem = value; }
        }

        public object SelectedItem
        {
            get {
                int selectedIndex = Control.SelectedItem;
                var comboItem = (selectedIndex >= 0 && selectedIndex < fItems.Count) ? fItems[selectedIndex] : null;
                return comboItem;
            }
            set {  }
        }

        public string Text
        {
            get { return Control.Text.ToString(); }
            set { Control.Text = value; }
        }

        public void Add(object item)
        {
            fItems.Add(new ComboItem<object>((string)item, item));
        }

        public void AddItem<T>(string caption, T tag, IImage image = null)
        {
            fItems.Add(new ComboItem<T>(caption, tag, image));
        }

        public void AddRange(IEnumerable<object> items, bool sorted = false)
        {
            fItems.Clear();
            foreach (var item in items) {
                fItems.Add(new ComboItem<object>(item.ToString(), item));
            }
        }

        public void AddStrings(StringList strings)
        {
            int num = strings.Count;
            for (int i = 0; i < num; i++) {
                AddItem(strings[i], strings.GetObject(i));
            }
        }

        public void BeginUpdate()
        {
        }

        public void Clear()
        {
            fItems.Clear();
        }

        public void EndUpdate()
        {
        }

        public void Sort()
        {
            fItems.Sort((x, y) => string.Compare(x.Text, y.Text, StringComparison.CurrentCulture));
        }

        public T GetSelectedTag<T>()
        {
            int selectedIndex = Control.SelectedItem;
            var comboItem = (selectedIndex >= 0 && selectedIndex < fItems.Count) ? fItems[selectedIndex] as ComboItem<T> : null;
            return (comboItem != null) ? comboItem.Tag : default;
        }

        public void SetSelectedTag<T>(T tagValue, bool allowDefault = true)
        {
            for (int i = 0; i < fItems.Count; i++) {
                if (fItems[i] is ComboItem<T> comboItem && object.Equals(comboItem.Tag, tagValue)) {
                    Control.SelectedItem = i;
                    return;
                }
            }

            if (allowDefault) {
                Control.SelectedItem = 0;
            }
        }
    }
}
