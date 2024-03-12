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
using System.Windows.Forms;
using BSLib;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    public sealed class ToolStripComboBoxHandler : ControlHandler<ToolStripComboBox, ToolStripComboBoxHandler>, IComboBox
    {
        public ToolStripComboBoxHandler(ToolStripComboBox control) : base(control)
        {
        }

        public bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
        }

        public IList Items
        {
            get { return Control.Items; }
        }

        public bool ReadOnly
        {
            get { return (Control.DropDownStyle == ComboBoxStyle.DropDownList); }
            set { Control.DropDownStyle = (value) ? ComboBoxStyle.DropDownList : ComboBoxStyle.DropDown; }
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
            get { return Control.Sorted; }
            set { Control.Sorted = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }

        public void Add(object item)
        {
            Control.Items.Add(item);
        }

        public void AddItem<T>(string text, T tag, IImage image = null)
        {
            Control.Items.Add(new ComboItem<T>(text, tag, image));
        }

        public void AddRange(IEnumerable<object> items, bool sorted = false)
        {
            Control.Items.Clear();
            Control.Sorted = false;
            foreach (object item in items) {
                Control.Items.Add(item);
            }
            Control.Sorted = sorted;
        }

        public void AddStrings(StringList strings)
        {
            int num = strings.Count;
            for (int i = 0; i < num; i++) {
                Control.Items.Add(strings[i]);
            }
        }

        public void BeginUpdate()
        {
            Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Items.Clear();
        }

        public void EndUpdate()
        {
            Control.EndUpdate();
        }

        public void Activate()
        {
            Control.Select();
        }

        public void Sort()
        {
        }

        public T GetSelectedTag<T>()
        {
            object selectedItem = Control.SelectedItem;
            ComboItem<T> comboItem = selectedItem as ComboItem<T>;
            T itemTag = (comboItem != null) ? comboItem.Tag : default(T);
            return itemTag;
        }

        public void SetSelectedTag<T>(T tagValue, bool allowDefault = true)
        {
            foreach (object item in Control.Items) {
                ComboItem<T> comboItem = item as ComboItem<T>;

                if (comboItem != null && object.Equals(comboItem.Tag, tagValue)) {
                    Control.SelectedItem = item;
                    return;
                }
            }

            if (allowDefault) {
                Control.SelectedIndex = 0;
            }
        }
    }
}
