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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Design.Views;
using GKCore.Options;

namespace GKUI.Forms
{
    public sealed partial class AboutDlg : CommonDialog, IAboutDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Label lblProduct;
        private Label lblVersion;
        private Label lblCopyright;
        private LinkButton lblMail;
        private Button btnClose;
        private LinkButton lblProjSite;
        private LinkButton lblForum;
        private LinkButton lblChannel;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public AboutDlg()
        {
            XamlReader.Load(this);

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

        private void LabelMail_Click(object sender, EventArgs e)
        {
            var lbl = sender as LinkButton;
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
