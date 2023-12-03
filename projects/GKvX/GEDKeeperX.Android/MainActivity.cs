using Android.App;
using Android.Content.PM;
using Android.OS;
using GKUI;

namespace GEDKeeperX.Droid
{
    [Activity(Label = "GEDKeeperX", Icon = "@mipmap/icon", Theme = "@style/MainTheme", MainLauncher = true, ConfigurationChanges = ConfigChanges.ScreenSize | ConfigChanges.Orientation, ScreenOrientation = ScreenOrientation.FullSensor)]
    public class MainActivity : global::Xamarin.Forms.Platform.Android.FormsAppCompatActivity, IPlatformSpecifics
    {
        protected override void OnCreate(Bundle savedInstanceState)
        {
            TabLayoutResource = Resource.Layout.Tabbar;
            ToolbarResource = Resource.Layout.Toolbar;

            base.OnCreate(savedInstanceState);
            global::Xamarin.Forms.Forms.Init(this, savedInstanceState);
            Xamarin.Essentials.Platform.Init(this, savedInstanceState);
            OxyPlot.Xamarin.Forms.Platform.Android.PlotViewRenderer.Init();
            CarouselView.FormsPlugin.Android.CarouselViewRenderer.Init();
            LoadApplication(new GKUI.App(this));
        }

        public override void OnRequestPermissionsResult(int requestCode, string[] permissions, Permission[] grantResults)
        {
            Xamarin.Essentials.Platform.OnRequestPermissionsResult(requestCode, permissions, grantResults);

            base.OnRequestPermissionsResult(requestCode, permissions, grantResults);
        }

        void IPlatformSpecifics.CloseApplication()
        {
            FinishAffinity();
        }

        string IPlatformSpecifics.GetExternalStorageDirectory()
        {
            // DirectoryDcim, DirectoryMovies, DirectoryMusic, DirectoryPictures, DirectoryScreenshots
            // DirectoryDocuments, DirectoryDownloads
            return Environment.GetExternalStoragePublicDirectory(Environment.DirectoryDownloads).AbsolutePath;
        }
    }
}
