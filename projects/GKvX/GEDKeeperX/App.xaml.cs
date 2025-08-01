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
