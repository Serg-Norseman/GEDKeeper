using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using GKUI.Forms;
using Windows.System;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media.Animation;
using Windows.UI.Xaml.Navigation;

namespace GKUI
{
    public sealed partial class MainPage : Page
    {
        // List of object holding the Navigation Tag and the relative Navigation Page 
        private readonly List<PageItem> fPages = new List<PageItem>
        {
            new PageItem("Home", typeof(HomePage)),
            new PageItem("Tools", typeof(ToolsPage)),

            new PageItem("About", typeof(AboutDlg), new SymbolIcon((Symbol)0xE15E)),
            new PageItem("AssociationEditDlg", typeof(AssociationEditDlg), new SymbolIcon((Symbol)0xE15E)),
            new PageItem("LanguageEditDlg", typeof(LanguageEditDlg), new SymbolIcon((Symbol)0xE15E)),
            new PageItem("LanguageSelectDlg", typeof(LanguageSelectDlg), new SymbolIcon((Symbol)0xE15E)),
            new PageItem("NoteEditDlg", typeof(NoteEditDlg), new SymbolIcon((Symbol)0xE15E)),
            new PageItem("PatriarchsViewer", typeof(PatriarchsViewerWin), new SymbolIcon((Symbol)0xE15E)),
            new PageItem("ProgressDlg", typeof(ProgressDlg), new SymbolIcon((Symbol)0xE15E)),
            new PageItem("QuickSearchDlg", typeof(QuickSearchDlg), new SymbolIcon((Symbol)0xE15E)),
            new PageItem("UserRef", typeof(UserRefEditDlg), new SymbolIcon((Symbol)0xE15E)),
        };

        public MainPage()
        {
            InitializeComponent();

            // Add keyboard accelerators for backwards navigation
            var goBack = new KeyboardAccelerator { Key = VirtualKey.GoBack };
            goBack.Invoked += BackInvoked;
            KeyboardAccelerators.Add(goBack);

            // ALT routes here
            var altLeft = new KeyboardAccelerator {
                Key = VirtualKey.Left,
                Modifiers = VirtualKeyModifiers.Menu
            };
            altLeft.Invoked += BackInvoked;
            KeyboardAccelerators.Add(altLeft);
        }

        private void FillNavViewItems()
        {
            // Add other items
            NavView.MenuItems.Add(new NavigationViewItemSeparator());
            for (int i = 2; i < fPages.Count; i++) {
                var pageItem = fPages[i];

                NavView.MenuItems.Add(new NavigationViewItem {
                    Content = pageItem.Tag,
                    Icon = pageItem.Symbol,
                    Tag = pageItem.Tag
                });
            }
        }

        #region Event handlers

        private void ContentFrame_NavigationFailed(object sender, NavigationFailedEventArgs e)
        {
            Debug.WriteLine("Failed to load Page " + e.SourcePageType.FullName);
        }

        private void NavView_Loaded(object sender, RoutedEventArgs e)
        {
            FillNavViewItems();

            // Load start Home page
            NavView.SelectedItem = NavView.MenuItems[0];
            NavView_Navigate("Home", new EntranceNavigationTransitionInfo());
        }

        private void NavView_ItemInvoked(NavigationView sender, NavigationViewItemInvokedEventArgs args)
        {
            if (args.IsSettingsInvoked == true) {
                NavView_Navigate("settings", new EntranceNavigationTransitionInfo());
            } else if (args.InvokedItem != null) {
                var navItemTag = args.InvokedItem.ToString();
                NavView_Navigate(navItemTag, new EntranceNavigationTransitionInfo());
            }
        }

        private void NavView_Navigate(string navItemTag, NavigationTransitionInfo transitionInfo)
        {
            Type _page = null;
            if (navItemTag == "settings") {
                _page = typeof(SettingsPage);
            } else {
                var item = fPages.FirstOrDefault(p => p.Tag.Equals(navItemTag));
                _page = item?.Page;
            }

            // Get the page type before navigation so you can prevent duplicate
            // entries in the backstack.
            var preNavPageType = ContentFrame.CurrentSourcePageType;

            // Only navigate if the selected page isn't currently loaded.
            if (!(_page is null) && !Type.Equals(preNavPageType, _page)) {
                ContentFrame.Navigate(_page, null, transitionInfo);
            }
        }

        private void NavView_BackRequested(NavigationView sender, NavigationViewBackRequestedEventArgs args)
        {
            On_BackRequested();
        }

        private void BackInvoked(KeyboardAccelerator sender, KeyboardAcceleratorInvokedEventArgs args)
        {
            On_BackRequested();
            args.Handled = true;
        }

        private bool On_BackRequested()
        {
            if (!ContentFrame.CanGoBack)
                return false;

            // Don't go back if the nav pane is overlayed.
            if (NavView.IsPaneOpen &&
                (NavView.DisplayMode == NavigationViewDisplayMode.Compact ||
                 NavView.DisplayMode == NavigationViewDisplayMode.Minimal))
                return false;

            ContentFrame.GoBack();
            return true;
        }

        private void On_Navigated(object sender, NavigationEventArgs e)
        {
            NavView.IsBackEnabled = ContentFrame.CanGoBack;

            if (ContentFrame.SourcePageType == typeof(SettingsPage)) {
                // SettingsItem is not part of NavView.MenuItems, and doesn't have a Tag.
                NavView.SelectedItem = (NavigationViewItem)NavView.SettingsItem;
                NavView.Header = "Settings";
            } else if (ContentFrame.SourcePageType != null) {
                var item = fPages.FirstOrDefault(p => p.Page == e.SourcePageType);
                var navItem = NavView.MenuItems.OfType<NavigationViewItem>().First(n => n.Tag.Equals(item.Tag));

                NavView.SelectedItem = navItem;
                NavView.Header = navItem?.Content?.ToString();
            }
        }

        #endregion
    }
}
