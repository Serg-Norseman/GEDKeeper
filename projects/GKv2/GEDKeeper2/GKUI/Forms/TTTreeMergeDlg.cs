/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTTreeMergeDlg : CommonDialog
    {
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;

        public TTTreeMergeDlg(IBaseWindow baseWin)
        {
            InitializeComponent();
            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fBase = baseWin;
            fTree = fBase.Context.Tree;

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MITreeTools);
            pageTreeMerge.Text = LangMan.LS(LSID.LSID_ToolOp_2);
            pageTreeMergeOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnTreeMerge.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            lblMasterBase.Text = LangMan.LS(LSID.LSID_MasterBase);
            lblOtherBase.Text = LangMan.LS(LSID.LSID_OtherBase);
            edMasterBase.Text = LangMan.LS(LSID.LSID_CurrentBase);
        }

        private void btnTreeMerge_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            edUpdateBase.Text = fileName;
            TreeTools.MergeTreeFile(fBase.Context.Tree, edUpdateBase.Text, mSyncRes);
            fBase.RefreshLists(false);
        }
    }
}
