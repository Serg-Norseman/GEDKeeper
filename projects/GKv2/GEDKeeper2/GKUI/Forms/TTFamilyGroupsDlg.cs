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
using System.Collections.Generic;
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
    public sealed partial class TTFamilyGroupsDlg : Form
    {
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;

        public TTFamilyGroupsDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fBase = baseWin;
            fTree = fBase.Context.Tree;

            tabsTools.SelectedIndex = 0;
            gkLogChart1.OnHintRequest += HintRequestEventHandler;

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
            pageFamilyGroups.Text = LangMan.LS(LSID.LSID_ToolOp_6);
            pageFamilyGroupsOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnCheckGroups.Text = LangMan.LS(LSID.LSID_CheckGroups);
        }

        private void CheckGroups()
        {
            IProgressController progress = AppHost.Progress;
            gkLogChart1.Clear();
            List<List<GEDCOMRecord>> treeFragments = TreeTools.SearchTreeFragments(fTree, progress);

            //progress.ProgressInit(LangMan.LS(LSID.LSID_CheckFamiliesConnection), fTree.RecordsCount);
            try {
                tvGroups.Nodes.Clear();

                int num = treeFragments.Count;
                for (int i = 0; i < num; i++) {
                    var groupRecords = treeFragments[i];

                    int cnt = groupRecords.Count;

                    TreeNode root = tvGroups.Nodes.Add(
                        (i + 1).ToString() + " " + LangMan.LS(LSID.LSID_Group).ToLower() + " (" + cnt.ToString() + ")");

                    for (int j = 0; j < cnt; j++) {
                        var iRec = (GEDCOMIndividualRecord)groupRecords[j];

                        string pn = GKUtils.GetNameString(iRec, true, false);
                        if (iRec.Patriarch) {
                            pn = "(*) " + pn;
                        }
                        root.Nodes.Add(new GKTreeNode(pn, iRec));
                    }
                    root.ExpandAll();

                    gkLogChart1.AddFragment(cnt);

                    //progress.ProgressStep();
                    Application.DoEvents();
                }
            } finally {
                treeFragments.Clear();
                //progress.ProgressDone();
            }
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

        private void btnCheckGroups_Click(object sender, EventArgs e)
        {
            CheckGroups();
        }
    }
}
