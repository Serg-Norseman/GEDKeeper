/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Views;
using GKCore.Options;

namespace GKUI.Forms
{
    public sealed partial class AboutDlg : CommonDialog, IAboutDlg
    {
        private readonly AboutDlgController fController;

        public AboutDlg()
        {
            InitializeComponent();

            fController = new AboutDlgController(this);
            fController.SetLocale();

            lblProduct.Text = GKData.APP_TITLE;
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

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        private void LabelMail_Click(object sender, EventArgs e)
        {
            var lbl = sender as Label;
            if (lbl != null) {
                GKUtils.LoadExtFile(lbl.Text);
            }
        }

        public override bool SkipTheme(IDisposable component)
        {
            if (component == lblProduct || component == lblVersion || component == lblCopyright ||
                component == lblMail || component == lblProjSite || component == lblForum || component == lblChannel) {
                return true;
            }
            return false;
        }
    }
}
