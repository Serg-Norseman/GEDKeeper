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

using System.Collections.Generic;
using System.Linq;
using DLToolkit.Forms.Controls;
using GKCore;
using Xamarin.Forms;
using Xamvvm;

namespace GKUI.Forms
{
    public partial class ServicesPage : ContentPage
    {
        private FlowPageModel fModel;

        public ServicesPage()
        {
            InitializeComponent();

            // https://metanit.com/sharp/xamarin/4.10.php

            var services = new List<FlowItem>();
            services.Add(new FlowItem("Services", "Organizer"));
            services.Add(new FlowItem("Services", "Slideshow"));
            services.Add(new FlowItem("Services", "Scripts"));
            services.Add(new FlowItem("Tools", "Compare"));
            services.Add(new FlowItem("Tools", "Merge"));
            services.Add(new FlowItem("Tools", "Split"));
            services.Add(new FlowItem("Tools", "Check"));
            services.Add(new FlowItem("Tools", "Search"));
            services.Add(new FlowItem("Tools", "Manage"));
            //services.Add(new FlowItem("", ""));

            fModel = new FlowPageModel();
            fModel.ReloadData(services);

            flowListView.FlowItemsSource = fModel.Items;
            flowListView.FlowItemTapped += FlowListView_FlowItemTapped;
        }

        private void FlowListView_FlowItemTapped(object sender, ItemTappedEventArgs e)
        {
            var item = e.Item as FlowItem;
            if (item != null)
                AppHost.StdDialogs.ShowMessage($"Tapped {item.Title}");
        }

        public void FlowScrollTo(object item)
        {
            flowListView.FlowScrollTo(item, ScrollToPosition.MakeVisible, true);
        }
    }


    public class FlowPageModel : BasePageModel
    {
        public FlowPageModel()
        {
        }

        public FlowObservableCollection<object> Items
        {
            get { return GetField<FlowObservableCollection<object>>(); }
            set { SetField(value); }
        }

        public void ReloadData(List<FlowItem> flowList)
        {
            var sorted = flowList
                .OrderBy(item => item.Title)
                .GroupBy(item => item.Group)
                .Select(itemGroup => new FlowItemsGroup<string, FlowItem>(itemGroup.Key, itemGroup))
                .ToList();

            Items = new FlowObservableCollection<object>(sorted);
        }
    }


    public class FlowItem : BaseModel
    {
        private string fGroup;

        public string Group
        {
            get { return fGroup; }
            set { SetField(ref fGroup, value); }
        }

        private string fTitle;

        public string Title
        {
            get { return fTitle; }
            set { SetField(ref fTitle, value); }
        }

        public Color Color { get; private set; } = Color.White;

        public FlowItem(string group, string title)
        {
            fGroup = group;
            fTitle = title;
        }
    }


    public class FlowItemsGroup<K, T> : FlowObservableCollection<T>
    {
        public K Key { get; private set; }

        public FlowItemsGroup(K key, IEnumerable<T> items)
        {
            Key = key;
            AddRange(items);
        }
    }
}
