/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using System.Collections.ObjectModel;
using BSLib;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Components;
using Xamarin.Forms;

namespace GKUI.Platform
{
    public sealed class ComboBoxHandler : BaseControlHandler<Picker, ComboBoxHandler>, IComboBox
    {
        private readonly IList fItems;

        public ComboBoxHandler(Picker control) : base(control)
        {
            fItems = new ObservableCollection<IComboItem>();
            control.ItemsSource = fItems;
        }

        public IList Items
        {
            get { return fItems; }
        }

        public bool ReadOnly
        {
            get { return true; }
            set { /*Control.ReadOnly = value;*/ }
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

        /*public object SelectedTag
        {
            get {
                return ((GKComboItem)Control.SelectedItem).Tag;
            }
            set {
                var ctl = Control;
                foreach (object item in ctl.Items) {
                    GKComboItem comboItem = (GKComboItem)item;
                    if (comboItem.Tag == value) {
                        ctl.SelectedItem = item;
                        return;
                    }
                }
                ctl.SelectedIndex = 0;
            }
        }*/

        public string Text
        {
            get { return /*Control.Text*/string.Empty; }
            set { /*Control.Text = value;*/ }
        }

        public void Add(object item)
        {
            AddItem<object>(item.ToString(), null);
        }

        public void AddItem<T>(string caption, T tag, IImage image = null)
        {
            fItems.Add(new GKComboItem<T>(caption, tag, image));
        }

        public void AddRange(IEnumerable<object> items, bool sorted = false)
        {
            //Control.Sorted = false;
            //Control.Items.AddRange(GKComboItem.Convert((string[])items));
            foreach (var itm in items) {
                fItems.Add(new GKComboItem<object>(itm.ToString(), null));
            }
            //Control.Sorted = sorted;
        }

        public void AddStrings(StringList strings)
        {
            for (int i = 0, num = strings.Count; i < num; i++) {
                fItems.Add(new GKComboItem<object>(strings[i], strings.GetObject(i)));
            }
        }

        public void BeginUpdate()
        {
            Control.ItemsSource = null;
        }

        public void Clear()
        {
            fItems.Clear();
        }

        public void EndUpdate()
        {
            Control.ItemsSource = fItems;
        }

        public void Sort()
        {
            //Control.SortItems();
        }

        public T GetSelectedTag<T>()
        {
            return default(T);
        }

        public void SetSelectedTag<T>(T tagValue, bool allowDefault = true)
        {
        }
    }
}
