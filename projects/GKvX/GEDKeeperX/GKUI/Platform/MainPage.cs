using System.Collections.Generic;
using System.Threading.Tasks;
using GKUI.Platform;
using Xamarin.Forms;

namespace GKUI.Forms
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

            var navPage = new NavigationPage(new AboutPage());
            if (Device.OS == TargetPlatform.iOS) {
                navPage.Icon = "tab_feed.png"; // FileImageSource
            }

            Master = new MenuPage();
            Detail = navPage;
        }

        public async Task NavigateFromMenu(int id)
        {
            if (!MenuPages.ContainsKey(id)) {
                switch (id) {
                    case (int)MenuItemType.Browse:
                        //MenuPages.Add(id, new NavigationPage(new ItemsPage()));
                        break;
                    case (int)MenuItemType.About:
                        MenuPages.Add(id, new NavigationPage(new AboutPage()));
                        break;
                    case (int)MenuItemType.UserRef:
                        MenuPages.Add(id, new NavigationPage(new UserRefEditDlg()));
                        break;
                    case (int)MenuItemType.QuickSearch:
                        MenuPages.Add(id, new NavigationPage(new QuickSearchDlg()));
                        break;
                    case (int)MenuItemType.Progress:
                        MenuPages.Add(id, new NavigationPage(new ProgressDlg()));
                        break;
                    case (int)MenuItemType.PatriarchsViewer:
                        MenuPages.Add(id, new NavigationPage(new PatriarchsViewerWin()));
                        break;
                    case (int)MenuItemType.LanguageSelect:
                        MenuPages.Add(id, new NavigationPage(new LanguageSelectDlg()));
                        break;
                    case (int)MenuItemType.BaseWinSDI:
                        MenuPages.Add(id, new NavigationPage(new BaseWinSDI()));
                        break;
                }
            }

            var newPage = MenuPages[id];

            if (newPage != null && Detail != newPage) {
                Detail = newPage;

                if (Device.RuntimePlatform == Device.Android)
                    await Task.Delay(100);

                IsPresented = false;
            }
        }
    }
}
