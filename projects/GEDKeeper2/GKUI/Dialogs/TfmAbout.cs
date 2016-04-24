/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon;
using GKCore;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmAbout : Form
    {
        public TfmAbout()
        {
            this.InitializeComponent();
            this.LabelCite.Text = GKData.APP_CITES;
            this.Text = LangMan.LS(LSID.LSID_MIAbout);
            this.btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
        }

        private void LabelMail_Click(object sender, EventArgs e)
        {
            GKUtils.LoadExtFile(this.LabelMail.Text);
        }

        public static void ShowAbout()
        {
            string copyright, version;
            GKUtils.GetAssemblyVersion(out copyright, out version);

            TfmAbout dlg = new TfmAbout();
            try
            {
                dlg.LabelProduct.Text = GKData.APP_TITLE;
                dlg.LabelVersion.Text = @"Version " + version;
                dlg.LabelCopyright.Text = copyright;
                dlg.ShowDialog();
            }
            finally
            {
                dlg.Dispose();
            }
        }

        private void LabelProductClick(object sender, EventArgs e)
        {
            string msg = SysInfo.GetPlatformID().ToString() + "/" + SysInfo.GetDesktopType().ToString() +
                "/IsUnix:" + SysInfo.IsUnix().ToString();
            GKUtils.ShowMessage(msg);
        }
    }
}
