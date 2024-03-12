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

using System;
using System.Collections;
using System.Collections.Generic;
using BSLib;
using Eto.Forms;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Components;

namespace GKUI.Platform.Handlers
{
    public sealed class ComboBoxHandler : BaseControlHandler<ComboBox, ComboBoxHandler>, IComboBox
    {
        public ComboBoxHandler(ComboBox control) : base(control)
        {
        }

        public new bool Enabled
        {
            get { return Control.Enabled; }
            set {
                Control.Enabled = value;
                SetAccessible(!Control.ReadOnly && Enabled);
            }
        }

        public IList Items
        {
            get { return Control.Items; }
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set {
                Control.ReadOnly = value;
                SetAccessible(!Control.ReadOnly && Enabled);
            }
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }

        public object SelectedItem
        {
            get { return Control.SelectedValue; }
            set { Control.SelectedValue = value; }
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
            set {
                Control.AutoComplete = true; // FIXME: Wrapper for EtoBug in ComboBox.setText
                Control.Text = value;
                Control.AutoComplete = false;
            }
        }

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }

        public void Add(object item)
        {
            Control.Items.Add((string)item);
        }

        public void AddItem<T>(string caption, T tag, IImage image = null)
        {
            Control.Items.Add(new GKComboItem<T>(caption, tag, image));
        }

        public void AddRange(IEnumerable<object> items, bool sorted = false)
        {
            Control.Items.Clear();
            //Control.Sorted = false;
            foreach (var item in items) {
                Control.Items.Add(new GKComboItem<object>(item.ToString(), item));
            }
            //Control.Sorted = sorted;
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
            //Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Items.Clear();
        }

        public void EndUpdate()
        {
            //Control.EndUpdate();
        }

        public void Sort()
        {
            Control.Items.Sort((x, y) => string.Compare(x.Text, y.Text, StringComparison.CurrentCulture));
        }

        public T GetSelectedTag<T>()
        {
            object selectedItem = Control.SelectedValue;
            GKComboItem<T> comboItem = selectedItem as GKComboItem<T>;
            T itemTag = comboItem != null ? comboItem.Tag : default;
            return itemTag;
        }

        public void SetSelectedTag<T>(T tagValue, bool allowDefault = true)
        {
            foreach (object item in Control.Items) {
                GKComboItem<T> comboItem = item as GKComboItem<T>;

                if (comboItem != null && Equals(comboItem.Tag, tagValue)) {
                    Control.SelectedValue = item;
                    return;
                }
            }

            if (allowDefault) {
                Control.SelectedIndex = 0;
            }
        }
    }
}
