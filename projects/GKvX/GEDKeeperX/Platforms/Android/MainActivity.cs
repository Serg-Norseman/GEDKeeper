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
    //[Activity(Label = "GEDKeeperX", Icon = "@mipmap/icon", Theme = "@style/MainTheme", MainLauncher = true, ConfigurationChanges = ConfigChanges.ScreenSize | ConfigChanges.Orientation, ScreenOrientation = ScreenOrientation.FullSensor)]
    [Activity(Theme = "@style/Maui.MainTheme.NoActionBar", MainLauncher = true,
          ConfigurationChanges = ConfigChanges.ScreenSize | ConfigChanges.Orientation | ConfigChanges.UiMode | ConfigChanges.ScreenLayout | ConfigChanges.SmallestScreenSize | ConfigChanges.Density)]
    public class MainActivity : MauiAppCompatActivity, IPlatformSpecifics
    {
        protected override void OnCreate(Bundle? savedInstanceState)
        {
            base.OnCreate(savedInstanceState);

            /*TabLayoutResource = Resource.Layout.Tabbar;
            ToolbarResource = Resource.Layout.Toolbar;
            global::Xamarin.Forms.Forms.Init(this, savedInstanceState);
            Xamarin.Essentials.Platform.Init(this, savedInstanceState);
            OxyPlot.Xamarin.Forms.Platform.Android.PlotViewRenderer.Init();
            CarouselView.FormsPlugin.Android.CarouselViewRenderer.Init();
            LoadApplication(new GKUI.App(this));*/
        }

        /*public override void OnRequestPermissionsResult(int requestCode, string[] permissions, Permission[] grantResults)
        {
            Xamarin.Essentials.Platform.OnRequestPermissionsResult(requestCode, permissions, grantResults);

            base.OnRequestPermissionsResult(requestCode, permissions, grantResults);
        }*/

        void IPlatformSpecifics.CloseApplication()
        {
            //FinishAffinity();
        }

        string IPlatformSpecifics.GetExternalStorageDirectory()
        {
            // DirectoryDcim, DirectoryMovies, DirectoryMusic, DirectoryPictures, DirectoryScreenshots
            // DirectoryDocuments, DirectoryDownloads
            return string.Empty; //Environment.GetExternalStoragePublicDirectory(Environment.DirectoryDownloads).AbsolutePath;
        }
    }
}
