using GKUI;
using Xamarin.Forms;

namespace GEDKeeperX.UWP
{
    public sealed partial class MainPage : IPlatformSpecifics
    {
        public MainPage()
        {
            InitializeComponent();

            LoadApplication(new GKUI.App(this));
        }

        void IPlatformSpecifics.CloseApplication()
        {
            Application.Current.Quit();
        }
    }
}
