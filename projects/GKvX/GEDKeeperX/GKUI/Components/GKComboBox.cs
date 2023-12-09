/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Collections.ObjectModel;
using System.Linq;
using BSLib;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class GKComboBox : Picker
    {
        private readonly Collection<IComboItem> fItems;
        private bool fReadOnly;
        private string fAppendNew;

        public new IList Items
        {
            get { return fItems; }
        }

        public bool ReadOnly
        {
            get { return fReadOnly; }
            set {
                fReadOnly = value;
                if (value) {
                    SelectedIndexChanged -= OnSelectedIndexChanged;
                    RemoveItem(fAppendNew);
                } else {
                    SelectedIndexChanged += OnSelectedIndexChanged;
                    Add(fAppendNew);
                }
            }
        }

        public string Text
        {
            get {
                var selItem = base.SelectedItem as IComboItem;
                return (selItem != null) ? selItem.Text : string.Empty;
            }
            set {
                base.SelectedItem = fItems.FirstOrDefault(x => x.Text == value);
            }
        }

        public event EventHandler Completed;
        public event EventHandler TextChanged;

        public GKComboBox()
        {
            fItems = new ObservableCollection<IComboItem>();
            ItemsSource = fItems;

            // for Xamarin Picker - default state
            fReadOnly = true;
            fAppendNew = string.Format("< {0} >", LangMan.LS(LSID.DlgAppend));
        }

        public void AddItem(string text)
        {
            AddItem<object>(text, null);
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
            ItemsSource = null;
        }

        public void Clear()
        {
            fItems.Clear();
        }

        public void EndUpdate()
        {
            ItemsSource = fItems;
        }

        private async void OnSelectedIndexChanged(object sender, EventArgs e)
        {
            var view = UIHelper.GetParentPage(this);
            if (Text == fAppendNew && view != null) {
                string newValue = await AppHost.StdDialogs.GetInput(view, LangMan.LS(LSID.Value), string.Empty);
                Add(newValue);
                Text = newValue;
            }
        }

        private void RemoveItem(string text)
        {
            var item = fItems.FirstOrDefault(x => x.Text == text);
            if (item != null) fItems.Remove(item);
        }
    }
}
