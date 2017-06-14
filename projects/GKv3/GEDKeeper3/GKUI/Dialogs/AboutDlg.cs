/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using GKCommon;
using GKCore;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class AboutDlg : ModalDialog
    {
        public AboutDlg()
        {
            InitializeComponent();

            btnClose.Image = Bitmap.FromResource("Resources.btn_accept.gif");

            Title = LangMan.LS(LSID.LSID_MIAbout);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            lblProduct.Text = GKData.APP_TITLE_NEW;
            lblVersion.Text = @"Version " + AppHost.Instance.GetAppVersion();
            lblCopyright.Text = AppHost.Instance.GetAppCopyright();
        }

        private void LabelMail_Click(object sender, EventArgs e)
        {
            var lbl = sender as LinkButton;
            if (lbl != null) {
                SysUtils.LoadExtFile(lbl.Text);
            }
        }
    }
}
