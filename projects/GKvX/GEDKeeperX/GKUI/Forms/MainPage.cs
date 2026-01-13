/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Threading.Tasks;
using GKCore;
using GKCore.Locales;
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
