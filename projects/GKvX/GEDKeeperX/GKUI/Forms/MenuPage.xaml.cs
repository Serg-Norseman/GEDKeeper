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
using GKCore;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public enum MenuItemType
    {
        Browse,
        About,
        UserRef,
        QuickSearch,
        Progress,
        PatriarchsViewer,
        LanguageSelect,
        Tests,
        Launch,
        Services,
        Exit,
    }


    public partial class MenuPage : ContentPage
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


        private readonly List<HomeMenuItem> fMenuItems;

        private MainPage RootPage
        {
            get {
                return Application.Current.MainPage as MainPage;
            }
        }

        public MenuPage()
        {
            InitializeComponent();

            fMenuItems = new List<HomeMenuItem>() {
                new HomeMenuItem (MenuItemType.Browse, "Browse"),
                new HomeMenuItem (MenuItemType.About, "About"),
                new HomeMenuItem (MenuItemType.UserRef, "UserRef"),
                new HomeMenuItem (MenuItemType.QuickSearch, "QuickSearch"),
                new HomeMenuItem (MenuItemType.Progress, "Progress"),
                new HomeMenuItem (MenuItemType.PatriarchsViewer, "PatriarchsViewer"),
                new HomeMenuItem (MenuItemType.LanguageSelect, "LanguageSelect"),
                new HomeMenuItem (MenuItemType.Tests, "Tests"),
                new HomeMenuItem (MenuItemType.Launch, "Launch"),
                new HomeMenuItem (MenuItemType.Services, "Services"),
                new HomeMenuItem (MenuItemType.Exit, "Exit"),
            };

            ListViewMenu.ItemsSource = fMenuItems;
            ListViewMenu.SelectedItem = fMenuItems[0];
            ListViewMenu.ItemSelected += async (sender, e) => {
                if (e.SelectedItem == null)
                    return;

                var id = (int)((HomeMenuItem)e.SelectedItem).Id;
                await RootPage.NavigateFromMenu(id);
            };
        }

        public static Page CreatePageInstance(int id)
        {
            Page result = null;
            switch ((MenuItemType)id) {
                case MenuItemType.Browse:
                    result = new BaseWinSDI();
                    break;
                case MenuItemType.About:
                    result = new AboutPage();
                    break;
                case MenuItemType.UserRef:
                    result = new UserRefEditDlg();
                    break;
                case MenuItemType.QuickSearch:
                    result = new QuickSearchDlg();
                    break;
                case MenuItemType.Progress:
                    result = new ProgressDlg();
                    break;
                case MenuItemType.PatriarchsViewer:
                    result = new PatriarchsViewerWin();
                    break;
                case MenuItemType.LanguageSelect:
                    result = new LanguageSelectDlg();
                    break;
                case MenuItemType.Tests:
                    result = new TestsPage();
                    break;
                case MenuItemType.Launch:
                    result = new LaunchPage();
                    break;
                case MenuItemType.Services:
                    result = new ServicesPage();
                    break;
                case MenuItemType.Exit:
                    AppHost.Instance.Quit();
                    break;
            }
            return result;
        }
    }
}
