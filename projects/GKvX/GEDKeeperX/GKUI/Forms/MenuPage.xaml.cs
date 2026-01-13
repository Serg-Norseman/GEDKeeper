/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.ObjectModel;
using GKCore;
using GKCore.Locales;
using GKUI.Platform;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public enum MenuItemType
    {
        Browse,
        About,
        Exit,
    }


    public partial class MenuPage : ContentPage, ILocalizable
    {
        private sealed class HomeMenuItem
        {
            public MenuItemType Id { get; set; }

            public string Title { get; set; }

            public HomeMenuItem(MenuItemType id, string title)
            {
                Id = id;
                Title = title;
            }
        }


        private readonly ObservableCollection<HomeMenuItem> fMenuItems;


        public MenuPage()
        {
            InitializeComponent();

            fMenuItems = new ObservableCollection<HomeMenuItem>();
            ReloadMenu();

            ListViewMenu.ItemsSource = fMenuItems;
            ListViewMenu.SelectedItem = fMenuItems[0];
            ListViewMenu.ItemSelected += async (sender, e) => {
                var item = e.SelectedItem as HomeMenuItem;
                if (item == null) return;
                await XFAppHost.GetMainPage().NavigateMenuAsync((int)item.Id);
            };
        }

        private void ReloadMenu()
        {
            fMenuItems.Clear();
            fMenuItems.Add(new HomeMenuItem(MenuItemType.Browse, LangMan.LS(LSID.RM_Records)));
            fMenuItems.Add(new HomeMenuItem(MenuItemType.About, LangMan.LS(LSID.MIAbout)));
            fMenuItems.Add(new HomeMenuItem(MenuItemType.Exit, LangMan.LS(LSID.MIExit)));
        }

        public static Page CreatePageInstance(int id)
        {
            Page result = null;
            switch ((MenuItemType)id) {
                case MenuItemType.Browse:
                    result = AppHost.Instance.GetCurrentFile() as Page;
                    break;
                case MenuItemType.About:
                    result = new AboutPage();
                    break;
                case MenuItemType.Exit:
                    AppHost.Instance.Quit();
                    break;
            }
            return result;
        }

        public void SetLocale()
        {
            ReloadMenu();
        }
    }
}
