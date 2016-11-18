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
using System.Diagnostics;
using System.Reflection;
using System.Windows.Forms;

using GKCommon;
using GKCore;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class AboutDlg : Form
    {
        public AboutDlg()
        {
            this.InitializeComponent();

            this.btnClose.Image = global::GKResources.iBtnAccept;

            this.Text = LangMan.LS(LSID.LSID_MIAbout);
            this.btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
        }

        private void LabelMail_Click(object sender, EventArgs e)
        {
            Label lbl = sender as Label;
            Process.Start(lbl.Text);
        }

        public static void ShowAbout()
        {
            string copyright, version;
            SysUtils.GetAssemblyVersion(Assembly.GetExecutingAssembly(), out copyright, out version);

            using (AboutDlg dlg = new AboutDlg())
            {
                dlg.lblProduct.Text = GKData.APP_TITLE;
                dlg.lblVersion.Text = @"Version " + version;
                dlg.lblCopyright.Text = copyright;
                dlg.ShowDialog();
            }
        }
    }
}
