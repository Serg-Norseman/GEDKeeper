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
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCore;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class DayTipsDlg : Form
    {
        private readonly StringList fTips;

        public DayTipsDlg()
        {
            this.InitializeComponent();

            this.Image1.Image = global::GKResources.iTipsLight;
            this.btnClose.Image = (Image)MainWin.ResourceManager.GetObjectEx("iBtnCancel");

            this.fTips = new StringList();

            // SetLang()
            this.btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            this.chkShow.Text = LangMan.LS(LSID.LSID_StartupTips);
            this.btnNextTip.Text = LangMan.LS(LSID.LSID_Next);
            this.lblTitle.Text = LangMan.LS(LSID.LSID_YouKnowWhat);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fTips.Dispose();
            }
            base.Dispose(disposing);
        }

        private void GetNextTip()
        {
            if (this.fTips.Count > 0)
            {
                this.txtTip.Text = this.fTips[0];
                this.fTips.Delete(0);
            }
            this.btnNextTip.Enabled = (this.fTips.Count > 0);
        }

        private void NextTipBtn_Click(object sender, EventArgs e)
        {
            this.GetNextTip();
        }

        /// <summary>
        /// Shows MOTD window.
        /// </summary>
        /// <param name="caption">Window title.</param>
        /// <param name="showTipsChecked">Initial state of the "Show on the
        /// application startup" option.</param>
        /// <param name="tips">List of messahes to show.</param>
        /// <param name="parent">handle to the parent window.</param>
        /// <returns>true if user wants to view MOTD window on the next
        /// application startup and false otherwise.</returns>
        public static bool ShowTipsEx(string caption, bool showTipsChecked,
                                      StringList tips, IntPtr parent)
        {
            bool result;
            using (DayTipsDlg dlg = new DayTipsDlg())
            {
                dlg.chkShow.Checked = showTipsChecked;
                dlg.Text = caption;
                dlg.lblTitle.Text = caption;
                dlg.fTips.Assign(tips);
                dlg.GetNextTip();

                UIManager.CenterFormByParent(dlg, parent);

                dlg.ShowDialog();

                result = dlg.chkShow.Checked;
            }
            return result;
        }
    }
}
