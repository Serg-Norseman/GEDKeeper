/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using BSLib;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Components;

namespace GKUI.Platform
{
    public sealed class PickerHandler : BaseControlHandler<GKComboBox, PickerHandler>, IComboBox
    {
        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set { Control.ReadOnly = value; }
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }

        public object SelectedItem
        {
            get { return Control.SelectedItem; }
            set { Control.SelectedItem = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public PickerHandler(GKComboBox control) : base(control)
        {
        }

        public void Add(object item)
        {
            Control.Add(item);
        }

        public void AddItem<T>(string caption, T tag, IImage image = null)
        {
            Control.AddItem(caption, tag, image);
        }

        public void AddRange(IEnumerable<object> items, bool sorted = false)
        {
            Control.AddRange(items, sorted);
        }

        public void AddStrings(StringList strings)
        {
            Control.AddStrings(strings);
        }

        public void BeginUpdate()
        {
            Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Clear();
        }

        public void EndUpdate()
        {
            Control.EndUpdate();
        }

        public void Sort()
        {
            //Control.SortItems();
        }

        public T GetSelectedTag<T>()
        {
            var selectedItem = SelectedItem as ComboItem<T>;
            return (selectedItem == null) ? default : selectedItem.Tag;
        }

        public void SetSelectedTag<T>(T tagValue, bool allowDefault = true)
        {
            foreach (object item in Control.Items) {
                var comboItem = item as ComboItem<T>;

                if (comboItem != null && Equals(comboItem.Tag, tagValue)) {
                    SelectedItem = item;
                    return;
                }
            }

            if (allowDefault) {
                Control.SelectedIndex = 0;
            }
        }
    }
}
