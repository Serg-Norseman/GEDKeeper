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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTFamilyGroupsDlg : EditorDialog, IFragmentSearchDlg
    {
        private readonly FragmentSearchController fController;

        #region View Interface

        ITreeViewHandler IFragmentSearchDlg.GroupsTree
        {
            get { return fControlsManager.GetControlHandler<ITreeViewHandler>(tvGroups); }
        }

        ILogChart IFragmentSearchDlg.LogChart
        {
            get { return fControlsManager.GetControlHandler<ILogChart>(gkLogChart1); }
        }

        #endregion

        public TTFamilyGroupsDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            InitDialog(baseWin);
            fController = new FragmentSearchController(this);
            fController.Init(baseWin);

            gkLogChart1.OnHintRequest += HintRequestEventHandler;

            SetLang();
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MITreeTools);
            pageFamilyGroups.Text = LangMan.LS(LSID.LSID_ToolOp_6);
            pageFamilyGroupsOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnAnalyseGroups.Text = LangMan.LS(LSID.LSID_Analysis);
        }

        private void btnAnalyseGroups_Click(object sender, EventArgs e)
        {
            fController.CheckGroups();
        }

        private void tvGroups_DoubleClick(object sender, EventArgs e)
        {
            GKTreeNode node = tvGroups.SelectedNode as GKTreeNode;
            if (node == null) return;

            GEDCOMIndividualRecord iRec = node.Tag as GEDCOMIndividualRecord;
            if (iRec == null) return;

            fBase.SelectRecordByXRef(iRec.XRef);
            Close();
        }

        private void HintRequestEventHandler(object sender, HintRequestEventArgs args)
        {
            if (args == null) return;

            args.Hint = string.Format(LangMan.LS(LSID.LSID_LogHint), args.FragmentNumber, args.Size);
        }
    }
}
