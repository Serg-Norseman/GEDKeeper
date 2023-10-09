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

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using GKCore;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class LaunchPage : ContentPage
    {
        public ObservableCollection<Grouping<string, LaunchItem>> LaunchItems { get; set; }

        public LaunchPage()
        {
            InitializeComponent();

            var launchItems = new List<LaunchItem>
            {
                new LaunchItem("File", "New"),
                new LaunchItem("File", "Load"),
                new LaunchItem("File", "Save"),
                new LaunchItem("File", "Close"),
                new LaunchItem("Recent", "Europe Kings.ged"),
                new LaunchItem("Recent", "My Kins.ged"),
                new LaunchItem("Edit", "Add")
            };

            var groups = launchItems.GroupBy(p => p.Group).Select(g => new Grouping<string, LaunchItem>(g.Key, g));
            LaunchItems = new ObservableCollection<Grouping<string, LaunchItem>>(groups);
            BindingContext = this;
        }

        private void Item_Tapped(object sender, EventArgs e)
        {
            var item = launchList.SelectedItem as LaunchItem;
            if (item != null)
                AppHost.StdDialogs.ShowMessage($"Tapped {item.Title}");
        }
    }

    /*
        <ListView x:Name="launchList" HasUnevenRows="True" ItemsSource="{Binding LaunchItems}" IsGroupingEnabled="True">
            <ListView.GroupHeaderTemplate>
                <DataTemplate>
                    <ViewCell>
                        <Label Text="{Binding Name}" FontSize="Large" Margin="8" />
                    </ViewCell>
                </DataTemplate>
            </ListView.GroupHeaderTemplate>
            <ListView.ItemTemplate>
                <DataTemplate>
                    <ViewCell>
                        <Label Text="{Binding Title}" Margin="6" />
                    </ViewCell>
                </DataTemplate>
            </ListView.ItemTemplate>
        </ListView>

        <CollectionView x:Name="launchList" ItemsSource="{Binding LaunchItems}" IsGrouped="true">
            <CollectionView.GroupHeaderTemplate>
                <DataTemplate>
                    <Label Text="{Binding Name}" TextColor="Orange" FontSize="Large" FontAttributes="Bold" Margin="8" />
                </DataTemplate>
            </CollectionView.GroupHeaderTemplate>
            <CollectionView.ItemTemplate>
                <DataTemplate>
                        <Label Text="{Binding Title}" Margin="6" />
                </DataTemplate>
            </CollectionView.ItemTemplate>
        </CollectionView>
     */

    public class Grouping<K, T> : ObservableCollection<T>
    {
        public K Name { get; private set; }
        public Grouping(K name, IEnumerable<T> items)
        {
            Name = name;
            foreach (T item in items)
                Items.Add(item);
        }
    }

    public class LaunchItem
    {
        public string Group { get; set; }
        public string Title { get; set; }

        public LaunchItem(string group, string title)
        {
            Group = group;
            Title = title;
        }
    }
}
