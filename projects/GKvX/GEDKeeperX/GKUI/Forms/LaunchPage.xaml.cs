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
using GKUI.Platform;
using Xamarin.CommunityToolkit.Extensions;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class LaunchPage : ContentPage
    {
        public ObservableCollection<Grouping<string, LaunchItem>> LaunchItems { get; set; }
        public ObservableCollection<Grouping<string, LaunchItem>> ServiceItems { get; set; }

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
                new LaunchItem("Edit", "Add"),

                new LaunchItem("Test", "OpenFile", async () => {
                    string fileName = await AppHost.StdDialogs.GetOpenFileAsync("title", "context", "GEDCOM Files|*.ged,*.gedz,*.gedsec", 0, "");
                    AppHost.StdDialogs.ShowAlert(fileName);
                }),
                new LaunchItem("Test", "LangSelect", async () => {
                    var curPage = Application.Current.MainPage;
                    curPage.Navigation.ShowPopup(new LanguageSelectDlg());
                }),
                new LaunchItem("Test", "Address", async () => {
                    XFAppHost.GetMainPage().NavigateAsync(new AddressEditDlg());
                }),
            };

            var groups = launchItems.GroupBy(p => p.Group).Select(g => new Grouping<string, LaunchItem>(g.Key, g));
            LaunchItems = new ObservableCollection<Grouping<string, LaunchItem>>(groups);

            var servicesItems = new List<LaunchItem>() {
                new LaunchItem("Services", "Organizer"),
                new LaunchItem("Services", "Slideshow"),
                new LaunchItem("Services", "Scripts"),
                new LaunchItem("Tools", "Compare"),
                new LaunchItem("Tools", "Merge"),
                new LaunchItem("Tools", "Split"),
                new LaunchItem("Tools", "Check"),
                new LaunchItem("Tools", "Search"),
                new LaunchItem("Tools", "Manage"),
            };

            var services = servicesItems.GroupBy(p => p.Group).Select(g => new Grouping<string, LaunchItem>(g.Key, g));
            ServiceItems = new ObservableCollection<Grouping<string, LaunchItem>>(services);

            BindingContext = this;
        }

        private async void lv_ItemSelected(object sender, SelectedItemChangedEventArgs e)
        {
            var item = e.SelectedItem as LaunchItem;
            item?.Action?.Invoke();
        }
    }

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
        public Action Action { get; set; }

        public LaunchItem(string group, string title)
        {
            Group = group;
            Title = title;
        }

        public LaunchItem(string group, string title, Action action)
        {
            Group = group;
            Title = title;
            Action = action;
        }
    }
}
