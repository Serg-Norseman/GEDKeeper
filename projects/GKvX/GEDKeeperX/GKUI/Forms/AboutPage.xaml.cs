/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Views;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class AboutPage : CommonDialog<IAboutDlg, AboutDlgController>, IAboutDlg
    {
        public AboutPage()
        {
            InitializeComponent();

            fController = new AboutDlgController(this);
            fController.SetLocale();
            fController.UpdateView();
        }

        private async void lblURL_Clicked(object sender, EventArgs e)
        {
            if (sender is Button btn)
                AppHost.Instance.OpenURL(btn.Text);
        }
    }
}
