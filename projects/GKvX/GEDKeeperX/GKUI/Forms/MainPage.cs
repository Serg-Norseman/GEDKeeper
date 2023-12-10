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

using System.Collections.Generic;
using System.Threading.Tasks;
using GKCore;
using GKCore.Interfaces;
using GKUI.Platform;
using Xamarin.Essentials;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public class MainPage : MasterDetailPage, ILocalizable
    {
        private readonly Dictionary<int, Page> fMenuPages;


        public MainPage()
        {
            MasterBehavior = MasterBehavior.Popover;
            Master = new MenuPage();
            Detail = new LaunchPage();

            fMenuPages = new Dictionary<int, Page>();

            DeviceDisplay.MainDisplayInfoChanged += DeviceDisplay_MainDisplayInfoChanged;
        }

        public static BaseWinSDI GetBaseWin()
        {
            return AppHost.Instance.GetCurrentFile() as BaseWinSDI;
        }

        private void DeviceDisplay_MainDisplayInfoChanged(object sender, DisplayInfoChangedEventArgs e)
        {
            ((IDisplayChangeable)Detail).OnDisplayChanged(e.DisplayInfo);
            ((IDisplayChangeable)GetBaseWin()).OnDisplayChanged(e.DisplayInfo);
        }

        public async Task NavigateAsync(Page page)
        {
            if (page == null) return;

            //IsPresented = false;
            await Navigation.PushAsync(page);
        }

        public async Task NavigateMenuAsync(int id)
        {
            Page page;
            if (!fMenuPages.TryGetValue(id, out page)) {
                page = MenuPage.CreatePageInstance(id);
                if (page != null) {
                    fMenuPages.Add(id, page);
                }
            }
            await NavigateAsync(page);
        }

        public void SetLocale()
        {
        }
    }
}
