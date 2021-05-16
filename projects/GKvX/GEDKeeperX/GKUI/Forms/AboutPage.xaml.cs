using System;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GKUI.Forms
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class AboutPage : ContentPage
    {
        public AboutPage()
        {
            InitializeComponent();
            Title = "About";
            LogoImage.Source = ImageSource.FromResource(@"Resources.gk_logo.png");
            AppNameLabel.Text = GKCore.GKData.APP_TITLE + " " + GKCore.AppHost.GetAppVersion();
        }

        private void OpenWeb_Clicked(object sender, EventArgs e)
        {
            Device.OpenUri(new Uri("https://gedkeeper.github.io/"));
        }
    }
}
