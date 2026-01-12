/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
