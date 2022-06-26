﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GKCore.Options;
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

            // FontSize="12" FontAttributes="Bold"

            Title = LangMan.LS(LSID.LSID_MIAbout);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            //lblProduct.Text = GKData.APP_TITLE;
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

        private void lblURL_Clicked(object sender, EventArgs e)
        {
            var btn = sender as Button;
            if (btn != null) {
                Device.OpenUri(new Uri(btn.Text));
            }
        }
    }
}
