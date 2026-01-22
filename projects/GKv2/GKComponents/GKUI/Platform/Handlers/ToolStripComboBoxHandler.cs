/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

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
