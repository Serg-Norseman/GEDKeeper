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

using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Tools;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTTreeSplitDlg : EditorDialog, ITreeSplitDlg
    {
        private readonly TreeSplitController fController;

        #region View Interface

        IListView ITreeSplitDlg.SelectedList
        {
            get { return ListSelected; }
        }

        IListView ITreeSplitDlg.SkippedList
        {
            get { return ListSkipped; }
        }

        #endregion

        public TTTreeSplitDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new TreeSplitController(this);
            fController.Init(baseWin);

            SetLang();
        }

        public void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_MITreeTools);
            pageTreeSplit.Text = LangMan.LS(LSID.LSID_ToolOp_3);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnSelectAll.Text = LangMan.LS(LSID.LSID_SelAll);
            btnSelectFamily.Text = LangMan.LS(LSID.LSID_SelFamily);
            btnSelectAncestors.Text = LangMan.LS(LSID.LSID_SelAncestors);
            btnSelectDescendants.Text = LangMan.LS(LSID.LSID_SelDescendants);
            btnDelete.Text = LangMan.LS(LSID.LSID_DoDelete);
            btnSave.Text = LangMan.LS(LSID.LSID_MIFileSaveAs);
        }

        private void btnSelectFamily_Click(object sender, EventArgs e)
        {
            fController.Select(fBase.GetSelectedPerson(), TreeTools.TreeWalkMode.twmFamily);
        }

        private void btnSelectAncestors_Click(object sender, EventArgs e)
        {
            fController.Select(fBase.GetSelectedPerson(), TreeTools.TreeWalkMode.twmAncestors);
        }

        private void btnSelectDescendants_Click(object sender, EventArgs e)
        {
            fController.Select(fBase.GetSelectedPerson(), TreeTools.TreeWalkMode.twmDescendants);
        }

        private void btnSelectAll_Click(object sender, EventArgs e)
        {
            fController.Select(fBase.GetSelectedPerson(), TreeTools.TreeWalkMode.twmAll);
        }

        private void btnDelete_Click(object sender, EventArgs e)
        {
            fController.Delete();
        }

        private void btnSave_Click(object sender, EventArgs e)
        {
            fController.Save();
        }
    }
}
