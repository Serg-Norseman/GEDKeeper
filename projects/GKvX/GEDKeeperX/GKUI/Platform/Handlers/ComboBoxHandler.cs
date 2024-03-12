/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using System.Collections;
using System.Collections.Generic;
using BSLib;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Components;

namespace GKUI.Platform
{
    public sealed class PickerHandler : BaseControlHandler<GKComboBox, PickerHandler>, IComboBox
    {
        public IList Items
        {
            get { return Control.Items; }
        }

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

        public bool Sorted
        {
            get { return false; }
            set {
                if (value) {
                    Sort();
                }
            }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public bool Visible
        {
            get { return Control.IsVisible; }
            set { Control.IsVisible = value; }
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
