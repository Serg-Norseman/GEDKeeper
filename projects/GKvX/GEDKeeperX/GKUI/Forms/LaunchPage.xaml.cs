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
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using GKCore;
using GKCore.Controllers;
using GKCore.Names;
using GKUI.Components;
using GKUI.Platform;
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
                new LaunchItem("File", "New", UIHelper.LoadResourceImage("Resources.btn_create_new.gif"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.NewFile();
                    XFAppHost.GetMainPage().NavigateAsync(baseWin);
                }),
                new LaunchItem("File", "Open", UIHelper.LoadResourceImage("Resources.btn_load.gif"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.LoadFileAsync();
                    XFAppHost.GetMainPage().NavigateAsync(baseWin);
                }),
                new LaunchItem("File", "Save", UIHelper.LoadResourceImage("Resources.btn_save.gif")),
                new LaunchItem("File", "Save As"),
                new LaunchItem("File", "Properties", UIHelper.LoadResourceImage("Resources.btn_properties.gif"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowFileProperties();
                }),

                new LaunchItem("Recent", "Europe Kings.ged"),
                new LaunchItem("Recent", "My Kins.ged"),

                new LaunchItem("Export", "Export table", UIHelper.LoadResourceImage("Resources.btn_excel.gif"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ExportTable();
                }),
                new LaunchItem("Export", "Book of Families", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ExportToFamilyBook();
                }),
                new LaunchItem("Export", "Album of Trees", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ExportToTreesAlbum();
                }),

                new LaunchItem("Help", "Content", UIHelper.LoadResourceImage("Resources.btn_help.gif"), async () => {
                    AppHost.Instance.ShowHelpTopic("");
                }),
                new LaunchItem("Help", "About", UIHelper.LoadResourceImage("Resources.btn_scroll.gif"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowAbout();
                }),

                new LaunchItem("Test", "QuickSearch", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    XFAppHost.GetMainPage().NavigateAsync(new QuickSearchDlg(baseWin));
                }),
                new LaunchItem("Test", "Progress", async () => {
                    XFAppHost.GetMainPage().NavigateAsync(new ProgressDlg());
                }),
                new LaunchItem("Test", "SexCheckDlg", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin.Context.DefineSex(baseWin, "Ivan", "Ivanovich");
                }),
                new LaunchItem("Test", "NameEditDlg", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    var nameEntry = new NameEntry();
                    BaseController.ModifyName(baseWin, baseWin.Context, ref nameEntry);
                }),
                new LaunchItem("Test", "PortraitSelectDlg", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    XFAppHost.GetMainPage().NavigateAsync(new PortraitSelectDlg(baseWin));
                }),
            };

            var groups = launchItems.GroupBy(p => p.Group).Select(g => new Grouping<string, LaunchItem>(g.Key, g));
            LaunchItems = new ObservableCollection<Grouping<string, LaunchItem>>(groups);

            var servicesItems = new List<LaunchItem>() {
                new LaunchItem("Pedigree", "Maps", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowMap();
                }),
                new LaunchItem("Pedigree", "Relationship Calculator", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowRelationshipCalculator();
                }),
                new LaunchItem("Pedigree", "Statistics", UIHelper.LoadResourceImage("Resources.btn_table.gif"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowStats();
                }),
                new LaunchItem("Services", "Organizer", UIHelper.LoadResourceImage("Resources.btn_organizer.gif"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowOrganizer();
                }),
                new LaunchItem("Services", "Slideshow", UIHelper.LoadResourceImage("Resources.btn_slideshow.png"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowSlideshow();
                }),
                new LaunchItem("Services", "Options", UIHelper.LoadResourceImage("Resources.btn_tools.gif"), async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    AppHost.Instance.ShowOptions(baseWin);
                }),
                new LaunchItem("Tools", "Compare databases", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowTreeCompare();
                }),
                new LaunchItem("Tools", "Merge databases", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowTreeMerge();
                }),
                new LaunchItem("Tools", "Split database", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowTreeSplit();
                }),
                new LaunchItem("Tools", "Merge records", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    BaseController.ShowRecMerge(baseWin, baseWin, null, null);
                }),
                new LaunchItem("Tools", "Check connection of families", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowFamilyGroups();
                }),
                new LaunchItem("Tools", "Check database", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowTreeCheck();
                }),
                new LaunchItem("Tools", "Search the patriarchs", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowPatSearch();
                }),
                new LaunchItem("Tools", "Manage places", async () => {
                    var baseWin = AppHost.Instance.GetCurrentFile() as BaseWinSDI;
                    baseWin?.Controller.ShowPlacesManager();
                }),
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
        public ImageSource Image { get; set; }
        public Action Action { get; set; }

        public LaunchItem(string group, string title)
        {
            Group = group;
            Title = title;
        }

        public LaunchItem(string group, string title, ImageSource image)
        {
            Group = group;
            Title = title;
            Image = image;
        }

        public LaunchItem(string group, string title, Action action)
        {
            Group = group;
            Title = title;
            Action = action;
        }

        public LaunchItem(string group, string title, ImageSource image, Action action)
        {
            Group = group;
            Title = title;
            Image = image;
            Action = action;
        }
    }
}
