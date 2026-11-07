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
using Microsoft.Maui.Controls;
using Microsoft.Maui.Devices;

namespace GKUI.Forms
{
    public class MainPage : FlyoutPage, ILocalizable
    {
        private readonly Dictionary<int, Page> fMenuPages;

        public NavigationPage NavigationPage
        {
            get { return Detail as NavigationPage; }
        }

        public MainPage()
        {
            FlyoutLayoutBehavior = FlyoutLayoutBehavior.Popover;
            Flyout = new MenuPage();
            Detail = new NavigationPage(new LaunchPage());

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
            if (page != null)
                await Navigation.PushAsync(page);
        }

        public void SetLocale()
        {
        }

        public async Task ShowAbout()
        {
            await GetBaseWin()?.Controller.ShowAbout();
        }
    }
}
