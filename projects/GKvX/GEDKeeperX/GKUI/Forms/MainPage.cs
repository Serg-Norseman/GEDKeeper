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
using GKCore.Design.Views;
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

        private void DeviceDisplay_MainDisplayInfoChanged(object sender, DisplayInfoChangedEventArgs e)
        {
            // all localizable/display-aware pages (LaunchPage, the work window)
            // live in the navigation stack; no casts of Detail or the current window
            var navPage = NavigationPage;
            if (navPage == null) return;

            foreach (var page in navPage.Navigation.NavigationStack) {
                if (page is IDisplayChangeable displayChangeable) {
                    displayChangeable.OnDisplayChanged(e.DisplayInfo);
                }
            }
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
            // About does not require a database; show it even without a work window
            using (var dlg = AppHost.Container.Resolve<IAboutDlg>()) {
                await AppHost.Instance.ShowModalAsync(dlg, null, false);
            }
        }
    }
}
