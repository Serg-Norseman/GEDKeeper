/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;
using GKUI.Forms;
using GKUI.Platform;
using Xamarin.Essentials;
using Xamarin.Forms;
using static Xamarin.Essentials.Permissions;

namespace GKUI
{
    public partial class App : Application
    {
        public App(IPlatformSpecifics platformSpecifics)
        {
            InitializeComponent();

            Xamarin.Forms.DataGrid.DataGridComponent.Init();

            LangMan.DefInit();
            XFAppHost.Startup(platformSpecifics);

            MainPage = new NavigationPage(new MainPage());
        }

        protected override void OnStart()
        {
            AppHost.InitSettings();
            AppHost.Instance.Init(null, false);

            RequestPermissions<Permissions.StorageRead>();
            RequestPermissions<Permissions.StorageWrite>();
        }

        private async static void RequestPermissions<T>() where T : BasePermission, new()
        {
            var status = await Permissions.CheckStatusAsync<T>();
            if (status != PermissionStatus.Granted) {
                status = await Permissions.RequestAsync<T>();
                if (status != PermissionStatus.Granted) {
                    string v = $"Permission must be ALLOW acceess to searching folder";
                    await App.Current.MainPage.DisplayAlert("", v, "OK");
                    await Permissions.RequestAsync<T>();
                }
            }
        }

        protected override void OnSleep()
        {
            AppHost.DoneSettings();
        }

        protected override void OnResume()
        {
        }
    }
}
