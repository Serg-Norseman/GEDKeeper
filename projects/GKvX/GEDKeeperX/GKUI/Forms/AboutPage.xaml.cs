/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GKCore;
using GKCore.Design.Views;
using GKCore.Options;
using Xamarin.Essentials;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class AboutPage : CommonDialog, IAboutDlg
    {
        public AboutPage()
        {
            InitializeComponent();

            Title = LangMan.LS(LSID.MIAbout);
            lblVersion.Text = @"Version " + AppHost.GetAppVersion();
            lblCopyright.Text = AppHost.GetAppCopyright();

            if (GlobalOptions.Instance.GetLanguageSign() == "rus") {
                lblForum.Text = GKData.APP_FORUM_RU;
                lblChannel.Text = GKData.APP_CHANNEL_RU;
            } else {
                lblForum.Text = GKData.APP_FORUM_EN;
                lblChannel.Text = GKData.APP_CHANNEL_EN;
            }
        }

        private async void lblURL_Clicked(object sender, EventArgs e)
        {
            var btn = sender as Button;
            if (btn != null) {
                AppHost.Instance.OpenURL(btn.Text);
            }
        }
    }
}
