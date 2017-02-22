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
            InitializeComponent();

            Image1.Image = GKResources.iTipsLight;
            btnClose.Image = GKResources.iBtnCancel;

            fTips = new StringList();

            // SetLang()
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            chkShow.Text = LangMan.LS(LSID.LSID_StartupTips);
            btnNextTip.Text = LangMan.LS(LSID.LSID_Next);
            lblTitle.Text = LangMan.LS(LSID.LSID_YouKnowWhat);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fTips.Dispose();
            }
            base.Dispose(disposing);
        }

        private void GetNextTip()
        {
            if (fTips.Count > 0)
            {
                string tip = fTips[0];

                // processing "title's directives"
                if (!string.IsNullOrEmpty(tip) && tip[0] == '#') {
                    tip = tip.Substring(1);
                    lblTitle.Text = tip;

                    fTips.Delete(0);
                    tip = fTips[0];
                }

                txtTip.Text = tip;
                fTips.Delete(0);
            }
            btnNextTip.Enabled = (fTips.Count > 0);
        }

        private void btnNextTip_Click(object sender, EventArgs e)
        {
            GetNextTip();
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

                UIHelper.CenterFormByParent(dlg, parent);

                dlg.ShowDialog();

                result = dlg.chkShow.Checked;
            }
            return result;
        }
    }
}
