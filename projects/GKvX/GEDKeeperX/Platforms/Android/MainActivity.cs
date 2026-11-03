/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Android.App;
using Android.Content.PM;
using Android.OS;
using GKUI;
using Microsoft.Maui;

namespace GEDKeeperX.Droid
{
    [Activity(Label = "GEDKeeperX", Theme = "@style/Maui.MainTheme.NoActionBar", MainLauncher = true,
          ConfigurationChanges =
            ConfigChanges.ScreenSize | ConfigChanges.Orientation | ConfigChanges.UiMode |
            ConfigChanges.ScreenLayout | ConfigChanges.SmallestScreenSize | ConfigChanges.Density, ScreenOrientation = ScreenOrientation.FullSensor)]
    public class MainActivity : MauiAppCompatActivity, IPlatformSpecifics
    {
        protected override void OnCreate(Bundle? savedInstanceState)
        {
            base.OnCreate(savedInstanceState);

            //LoadApplication(new GKUI.App(this));
        }

        public override void OnRequestPermissionsResult(int requestCode, string[] permissions, Permission[] grantResults)
        {
            base.OnRequestPermissionsResult(requestCode, permissions, grantResults);
        }

        void IPlatformSpecifics.CloseApplication()
        {
            FinishAffinity();
        }

        string IPlatformSpecifics.GetExternalStorageDirectory()
        {
            return Environment.GetExternalStoragePublicDirectory(Environment.DirectoryDownloads).AbsolutePath;
        }
    }
}
