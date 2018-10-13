using System;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GEDKeeperX.Views
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class AboutPage : ContentPage
    {
        public AboutPage()
        {
            InitializeComponent();
            Title = "About";
            LogoImage.Source = ImageSource.FromResource(@"Resources.gk_logo.png");
            AppNameLabel.Text = GKCore.GKData.APP_TITLE_NEW + " " + GKCore.AppHost.GetAppVersion();
        }

        async void OpenWeb_Clicked(object sender, EventArgs e)
        {
            Device.OpenUri(new Uri("https://gedkeeper.github.io/"));
        }
    }
}
