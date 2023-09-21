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
using System.Threading.Tasks;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class MainPage : MasterDetailPage
    {
        private readonly Dictionary<int, NavigationPage> fMenuPages;
        private Page fCurrentPage;

        public Page CurrentPage
        {
            get { return fCurrentPage; }
            set {
                fCurrentPage = value;
                Detail = fCurrentPage;
            }
        }

        public MainPage()
        {
            MasterBehavior = MasterBehavior.Popover;

            var navPage = new NavigationPage(new BaseWinSDI());
            if (Device.RuntimePlatform == Device.iOS) {
                navPage.Icon = "tab_feed.png"; // FileImageSource
            }

            Master = new MenuPage();
            CurrentPage = navPage;

            fMenuPages = new Dictionary<int, NavigationPage>();
            fMenuPages.Add((int)MenuItemType.Browse, navPage);
        }

        public void NavigateEx(Page page)
        {
            if (page == null) return;

            CurrentPage = new NavigationPage(page);
            IsPresented = false;
        }

        public async Task NavigateFromMenu(int id)
        {
            NavigationPage newPage;
            if (!fMenuPages.TryGetValue(id, out newPage)) {
                var page = MenuPage.CreatePageInstance(id);
                if (page != null) {
                    newPage = new NavigationPage(page);
                    fMenuPages.Add(id, newPage);
                }
            }

            if (newPage != null && Detail != newPage) {
                CurrentPage = newPage;

                if (Device.RuntimePlatform == Device.Android)
                    await Task.Delay(100);

                IsPresented = false;
            }
        }
    }
}
