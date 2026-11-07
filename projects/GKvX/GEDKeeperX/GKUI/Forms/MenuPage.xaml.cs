/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.ObjectModel;
using System.Diagnostics;
using GKCore;
using GKCore.Locales;
using GKUI.Platform;
using Microsoft.Maui.Controls;

namespace GKUI.Forms
{
    public partial class MenuPage : ContentPage, ILocalizable
    {
        private readonly ObservableCollection<LaunchItem> fMenuItems;


        public MenuPage()
        {
            InitializeComponent();

            fMenuItems = new ObservableCollection<LaunchItem>();
            ReloadMenu();

            ListViewMenu.ItemsSource = fMenuItems;
            ListViewMenu.SelectedItem = fMenuItems[0];
            ListViewMenu.ItemSelected += async (sender, e) => {
                try {
                    if (e.SelectedItem is LaunchItem item) {
                        // Deselect the list item immediately so that
                        // the ListView doesn't get stuck in a "pressed" state
                        ((ListView)sender).SelectedItem = null;

                        await item.Action?.Invoke(item);
                    }
                } catch (Exception ex) {
                    Trace.WriteLine("ListViewMenu.ItemSelected(): " + ex.ToString());
                }
            };
        }

        private void ReloadMenu()
        {
            fMenuItems.Clear();

            fMenuItems.Add(new LaunchItem(LangMan.LS(LSID.RM_Records), async (i) => {
                await XFAppHost.GetMainPage().NavigateAsync(LaunchPage.GetBaseWin());
            }));

            fMenuItems.Add(new LaunchItem(LangMan.LS(LSID.MIAbout), async (i) => {
                await XFAppHost.GetMainPage()?.ShowAbout();
            }));

            fMenuItems.Add(new LaunchItem(LangMan.LS(LSID.MIExit), async (i) => {
                AppHost.Instance.Quit();
            }));
        }

        public void SetLocale()
        {
            ReloadMenu();
        }
    }
}
