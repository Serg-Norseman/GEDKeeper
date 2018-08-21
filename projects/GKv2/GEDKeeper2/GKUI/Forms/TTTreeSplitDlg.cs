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
using System.IO;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTTreeSplitDlg : Form
    {
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;
        private readonly List<GEDCOMRecord> fSplitList;

        public TTTreeSplitDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fBase = baseWin;
            fTree = fBase.Context.Tree;

            tabsTools.SelectedIndex = 0;

            fSplitList = new List<GEDCOMRecord>();

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //fSplitList.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MITreeTools);
            pageTreeSplit.Text = LangMan.LS(LSID.LSID_ToolOp_3);
            pageTreeSplitOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnSelectAll.Text = LangMan.LS(LSID.LSID_SelAll);
            btnSelectFamily.Text = LangMan.LS(LSID.LSID_SelFamily);
            btnSelectAncestors.Text = LangMan.LS(LSID.LSID_SelAncestors);
            btnSelectDescendants.Text = LangMan.LS(LSID.LSID_SelDescendants);
            btnDelete.Text = LangMan.LS(LSID.LSID_DoDelete);
            btnSave.Text = LangMan.LS(LSID.LSID_MIFileSaveAs);
        }

        private void UpdateSplitLists()
        {
            ListSelected.BeginUpdate();
            ListSelected.Items.Clear();
            ListSkipped.BeginUpdate();
            ListSkipped.Items.Clear();
            try {
                int cnt = 0;
                int num = fTree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GEDCOMRecord rec = fTree[i];
                    if (rec is GEDCOMIndividualRecord) {
                        cnt++;
                        GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
                        string st = iRec.XRef + " / " + GKUtils.GetNameString(iRec, true, false);

                        if (fSplitList.IndexOf(iRec) < 0) {
                            ListSkipped.Items.Add(st);
                        } else {
                            ListSelected.Items.Add(st);
                        }
                    }
                }
                Text = fSplitList.Count.ToString() + @" / " + cnt.ToString();
            } finally {
                ListSelected.EndUpdate();
                ListSkipped.EndUpdate();
            }
        }

        private void Select(GEDCOMIndividualRecord startPerson, TreeTools.TreeWalkMode walkMode)
        {
            fSplitList.Clear();

            if (startPerson == null) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
            } else {
                TreeTools.WalkTree(startPerson, walkMode, fSplitList);
            }

            UpdateSplitLists();
        }

        private void btnSelectFamily_Click(object sender, EventArgs e)
        {
            Select(fBase.GetSelectedPerson(), TreeTools.TreeWalkMode.twmFamily);
        }

        private void btnSelectAncestors_Click(object sender, EventArgs e)
        {
            Select(fBase.GetSelectedPerson(), TreeTools.TreeWalkMode.twmAncestors);
        }

        private void btnSelectDescendants_Click(object sender, EventArgs e)
        {
            Select(fBase.GetSelectedPerson(), TreeTools.TreeWalkMode.twmDescendants);
        }

        private void btnSelectAll_Click(object sender, EventArgs e)
        {
            Select(fBase.GetSelectedPerson(), TreeTools.TreeWalkMode.twmAll);
        }

        private void btnDelete_Click(object sender, EventArgs e)
        {
            int num = fSplitList.Count;
            if (num == 0) return;

            for (int i = 0; i < num; i++) {
                object obj = fSplitList[i];

                if (obj is GEDCOMIndividualRecord) {
                    BaseController.DeleteRecord(fBase, obj as GEDCOMIndividualRecord, false);
                }
            }

            fSplitList.Clear();
            UpdateSplitLists();
            fBase.RefreshLists(false);

            AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.LSID_RecsDeleted));
        }

        private void btnSave_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT, "");
            if (string.IsNullOrEmpty(fileName)) return;

            TreeTools.CheckRelations(fSplitList);

            GKUtils.PrepareHeader(fTree, fileName, GlobalOptions.Instance.DefCharacterSet, true);

            using (StreamWriter fs = new StreamWriter(fileName, false, GEDCOMUtils.GetEncodingByCharacterSet(fTree.Header.CharacterSet))) {
                var gedcomProvider = new GEDCOMProvider(fTree);
                gedcomProvider.SaveToStream(fs, fSplitList);

                fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
            }
        }
    }
}
