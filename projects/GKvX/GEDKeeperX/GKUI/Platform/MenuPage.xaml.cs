using System.Collections.Generic;
using GKUI.Platform;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GKUI.Forms
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class MenuPage : ContentPage
    {
        private readonly List<HomeMenuItem> fMenuItems;

        private MainPage RootPage
        {
            get {
                return Application.Current.MainPage as MainPage;
            }
        }

        public MenuPage()
        {
            InitializeComponent();

            fMenuItems = new List<HomeMenuItem>
            {
                new HomeMenuItem {Id = MenuItemType.Browse, Title="Browse" },
                new HomeMenuItem {Id = MenuItemType.About, Title="About" },
                new HomeMenuItem {Id = MenuItemType.UserRef, Title="UserRef" },
                new HomeMenuItem {Id = MenuItemType.QuickSearch, Title="QuickSearch" },
                new HomeMenuItem {Id = MenuItemType.Progress, Title="Progress" },
                new HomeMenuItem {Id = MenuItemType.PatriarchsViewer, Title="PatriarchsViewer" },
            };

            ListViewMenu.ItemsSource = fMenuItems;
            ListViewMenu.SelectedItem = fMenuItems[0];
            ListViewMenu.ItemSelected += async (sender, e) =>
            {
                if (e.SelectedItem == null)
                    return;

                var id = (int)((HomeMenuItem)e.SelectedItem).Id;
                await RootPage.NavigateFromMenu(id);
            };
        }
    }
}
