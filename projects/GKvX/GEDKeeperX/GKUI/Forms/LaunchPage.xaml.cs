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
using GKCore.Interfaces;
using GKUI.Components;
using GKUI.Platform;
using Xamarin.Essentials;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class LaunchPage : ContentPage, ILocalizable, IDisplayChangeable
    {
        public ObservableCollection<Grouping<string, LaunchItem>> LaunchItems { get; set; }
        public ObservableCollection<Grouping<string, LaunchItem>> ServiceItems { get; set; }

        public LaunchPage()
        {
            InitializeComponent();

            ResetView();
        }

        private void ResetView()
        {
            var launchItems = new List<LaunchItem>
            {
                new LaunchItem(LangMan.LS(LSID.MIFile), LangMan.LS(LSID.MIFileNew), UIHelper.LoadResourceImage("Resources.btn_create_new.gif"), async (item) => {
                    var baseWin = GetBaseWin();
                    baseWin?.Controller.NewFile();
                    await XFAppHost.GetMainPage().NavigateAsync(baseWin);
                }),
                new LaunchItem(LangMan.LS(LSID.MIFile), LangMan.LS(LSID.MIFileLoad), UIHelper.LoadResourceImage("Resources.btn_load.gif"), async (item) => {
                    var baseWin = GetBaseWin();
                    baseWin?.Controller.LoadFileEx();
                    await XFAppHost.GetMainPage().NavigateAsync(baseWin);
                }),
                new LaunchItem(LangMan.LS(LSID.MIFile), LangMan.LS(LSID.MIFileSave), UIHelper.LoadResourceImage("Resources.btn_save.gif")),
                new LaunchItem(LangMan.LS(LSID.MIFile), LangMan.LS(LSID.MIFileSaveAs)),
                new LaunchItem(LangMan.LS(LSID.MIFile), LangMan.LS(LSID.MIFileProperties), UIHelper.LoadResourceImage("Resources.btn_properties.gif"), (item) => {
                    GetBaseWin()?.Controller.ShowFileProperties();
                }),

                new LaunchItem("Recent", "Sample.ged"), // stub for position hold

                new LaunchItem(LangMan.LS(LSID.MIExport), LangMan.LS(LSID.ExportTable), UIHelper.LoadResourceImage("Resources.btn_excel.gif"), (item) => {
                    GetBaseWin()?.Controller.ExportTable();
                }),
                new LaunchItem(LangMan.LS(LSID.MIExport), LangMan.LS(LSID.FamilyBook), (item) => {
                    GetBaseWin()?.Controller.ExportToFamilyBook();
                }),
                new LaunchItem(LangMan.LS(LSID.MIExport), LangMan.LS(LSID.TreesAlbum), (item) => {
                    GetBaseWin()?.Controller.ExportToTreesAlbum();
                }),

                new LaunchItem(LangMan.LS(LSID.MIHelp), LangMan.LS(LSID.LogView), (item) => {
                    GetBaseWin()?.Controller.ShowLog();
                }),
                new LaunchItem(LangMan.LS(LSID.MIHelp), LangMan.LS(LSID.TableOfContents), UIHelper.LoadResourceImage("Resources.btn_help.gif"), (item) => {
                    AppHost.Instance.ShowHelpTopic("");
                }),
                new LaunchItem(LangMan.LS(LSID.MIHelp), LangMan.LS(LSID.MIAbout), UIHelper.LoadResourceImage("Resources.btn_scroll.gif"), (item) => {
                    GetBaseWin()?.Controller.ShowAbout();
                }),
            };

            var mruFiles = AppHost.Options.MRUFiles;
            for (int i = 0, num = mruFiles.Count; i < num; i++) {
                string fn = mruFiles[i].FileName;

                launchItems.Add(new LaunchItem("Recent", fn, async (item) => {
                    var baseWin = GetBaseWin();
                    await AppHost.Instance.LoadBase(baseWin, item.Title);
                }));
            }

            var servicesItems = new List<LaunchItem>() {
                new LaunchItem(LangMan.LS(LSID.MIPedigree), LangMan.LS(LSID.MIMap), (item) => {
                    GetBaseWin()?.Controller.ShowMap();
                }),
                new LaunchItem(LangMan.LS(LSID.MIPedigree), LangMan.LS(LSID.RelationshipCalculator), (item) => {
                    GetBaseWin()?.Controller.ShowRelationshipCalculator();
                }),
                new LaunchItem(LangMan.LS(LSID.MIPedigree), LangMan.LS(LSID.MIStats), UIHelper.LoadResourceImage("Resources.btn_chart.gif"), (item) => {
                    GetBaseWin()?.Controller.ShowStats();
                }),

                new LaunchItem(LangMan.LS(LSID.MIService), LangMan.LS(LSID.MIOrganizer), UIHelper.LoadResourceImage("Resources.btn_organizer.gif"), (item) => {
                    GetBaseWin()?.Controller.ShowOrganizer();
                }),
                new LaunchItem(LangMan.LS(LSID.MIService), LangMan.LS(LSID.Slideshow), UIHelper.LoadResourceImage("Resources.btn_slideshow.png"), (item) => {
                    GetBaseWin()?.Controller.ShowSlideshow();
                }),
                new LaunchItem(LangMan.LS(LSID.MIService), LangMan.LS(LSID.MIOptions), UIHelper.LoadResourceImage("Resources.btn_tools.gif"), (item) => {
                    var baseWin = GetBaseWin();
                    AppHost.Instance.ShowOptions(baseWin);
                }),

                new LaunchItem(LangMan.LS(LSID.MITreeTools), LangMan.LS(LSID.TreeCompare), (item) => {
                    GetBaseWin()?.Controller.ShowTreeCompare();
                }),
                new LaunchItem(LangMan.LS(LSID.MITreeTools), LangMan.LS(LSID.TreeMerge), (item) => {
                    GetBaseWin()?.Controller.ShowTreeMerge();
                }),
                new LaunchItem(LangMan.LS(LSID.MITreeTools), LangMan.LS(LSID.TreeSplit), (item) => {
                    GetBaseWin()?.Controller.ShowTreeSplit();
                }),
                new LaunchItem(LangMan.LS(LSID.MITreeTools), LangMan.LS(LSID.MergeDuplicates), (item) => {
                    var baseWin = GetBaseWin();
                    BaseController.ShowRecMerge(baseWin, baseWin, null, null);
                }),
                new LaunchItem(LangMan.LS(LSID.MITreeTools), LangMan.LS(LSID.FragmentSearch), (item) => {
                    GetBaseWin()?.Controller.ShowFamilyGroups();
                }),
                new LaunchItem(LangMan.LS(LSID.MITreeTools), LangMan.LS(LSID.TreeCheck), (item) => {
                    GetBaseWin()?.Controller.ShowTreeCheck();
                }),
                new LaunchItem(LangMan.LS(LSID.MITreeTools), LangMan.LS(LSID.PatriarchsSearch), (item) => {
                    GetBaseWin()?.Controller.ShowPatSearch();
                }),
                new LaunchItem(LangMan.LS(LSID.MITreeTools), LangMan.LS(LSID.PlacesManager), (item) => {
                    GetBaseWin()?.Controller.ShowPlacesManager();
                }),
            };

            BindingContext = this;

            var orientation = DeviceDisplay.MainDisplayInfo.Orientation;
            if (orientation == DisplayOrientation.Landscape) {
                LaunchItems = PrepareItems(launchItems);
                ServiceItems = PrepareItems(servicesItems);

                launchList.IsVisible = true;
                servicesList.IsVisible = true;
            } else {
                launchItems.AddRange(servicesItems);

                LaunchItems = PrepareItems(launchItems);
                ServiceItems = null;

                launchList.IsVisible = true;
                servicesList.IsVisible = false;
            }

            var recent = LaunchItems.FirstOrDefault(x => x.Name == "Recent");
            if (recent != null) {
                var stubItem = recent.FirstOrDefault(x => x.Title == "Sample.ged");
                if (stubItem != null) {
                    recent.Remove(stubItem);
                }
            }

            launchList.ItemsSource = LaunchItems;
            servicesList.ItemsSource = ServiceItems;
        }

        private void lv_ItemSelected(object sender, SelectedItemChangedEventArgs e)
        {
            var item = e.SelectedItem as LaunchItem;
            ((ListView)sender).SelectedItem = null;
            item?.Action?.Invoke(item);
        }

        private static ObservableCollection<Grouping<string, LaunchItem>> PrepareItems(List<LaunchItem> items)
        {
            var groups = items.GroupBy(p => p.Group).Select(g => new Grouping<string, LaunchItem>(g.Key, g));
            return new ObservableCollection<Grouping<string, LaunchItem>>(groups);
        }

        private static BaseWinSDI GetBaseWin()
        {
            return AppHost.Instance.GetCurrentFile() as BaseWinSDI;
        }

        public void SetLocale()
        {
            ResetView();
        }

        void IDisplayChangeable.OnDisplayChanged(DisplayInfo displayInfo)
        {
            ResetView();
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
        public Action<LaunchItem> Action { get; set; }

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

        public LaunchItem(string group, string title, Action<LaunchItem> action)
        {
            Group = group;
            Title = title;
            Action = action;
        }

        public LaunchItem(string group, string title, ImageSource image, Action<LaunchItem> action)
        {
            Group = group;
            Title = title;
            Image = image;
            Action = action;
        }
    }
}
