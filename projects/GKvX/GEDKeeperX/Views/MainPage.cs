using GEDKeeperX.XModel;
using GKUI.Forms;
using System.Collections.Generic;
using System.Threading.Tasks;
using Xamarin.Forms;

namespace GEDKeeperX.Views
{
    public partial class MainPage : MasterDetailPage
    {
        private Dictionary<int, NavigationPage> MenuPages = new Dictionary<int, NavigationPage>();

        public MainPage()
        {
            InitializeComponent();

            MenuPages.Add((int)MenuItemType.Browse, (NavigationPage)Detail);
        }

        private void InitializeComponent()
        {
            MasterBehavior = MasterBehavior.Popover;

            var navPage = new NavigationPage(new ItemsPage());
            if (Device.OS == TargetPlatform.iOS)
            {
                navPage.Icon = "tab_feed.png"; // FileImageSource
            }

            Master = new GEDKeeperX.Views.MenuPage();
            Detail = navPage;
        }

        public async Task NavigateFromMenu(int id)
        {
            if (!MenuPages.ContainsKey(id))
            {
                switch (id)
                {
                    case (int)MenuItemType.Browse:
                        MenuPages.Add(id, new NavigationPage(new ItemsPage()));
                        break;
                    case (int)MenuItemType.About:
                        MenuPages.Add(id, new NavigationPage(new AboutPage()));
                        break;
                    case (int)MenuItemType.UserRef:
                        MenuPages.Add(id, new NavigationPage(new UserRefEditDlg()));
                        break;
                }
            }

            var newPage = MenuPages[id];

            if (newPage != null && Detail != newPage)
            {
                Detail = newPage;

                if (Device.RuntimePlatform == Device.Android)
                    await Task.Delay(100);

                IsPresented = false;
            }
        }
    }
}
