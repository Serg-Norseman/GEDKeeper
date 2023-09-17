/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using GKUI.Components;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GKUI.Forms
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class TestsPage : ContentPage
    {
        public TestsPage()
        {
            InitializeComponent();
        }

        private void btnAlert_Clicked(object sender, EventArgs e)
        {
            AppHost.StdDialogs.ShowAlert("Test Alert!");
        }

        private async void btnQuestion_Clicked(object sender, EventArgs e)
        {
            if (await AppHost.StdDialogs.ShowQuestionAsync("Question"))
                AppHost.StdDialogs.ShowAlert("Yes");
        }

        private async void btnOpenFile_Clicked(object sender, EventArgs e)
        {
            string fileName = await AppHost.StdDialogs.GetOpenFileAsync("title", "context", "GEDCOM Files|*.ged,*.gedz,*.gedsec", 0, "");
            AppHost.StdDialogs.ShowAlert(fileName);
        }

        private void btnCopyToCb_Clicked(object sender, EventArgs e)
        {
            UIHelper.SetClipboardText("clipboard sample");
        }
    }
}
