/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.IO;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Tools;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class TreeSplitController : DialogController<ITreeSplitDlg>
    {
        private readonly List<GDMRecord> fSplitList;

        public TreeSplitController(ITreeSplitDlg view) : base(view)
        {
            fSplitList = new List<GDMRecord>();
        }

        public override void UpdateView()
        {
            fView.SelectedList.BeginUpdate();
            fView.SelectedList.ClearItems();
            fView.SkippedList.BeginUpdate();
            fView.SkippedList.ClearItems();
            try {
                var tree = fBase.Context.Tree;
                int cnt = 0;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = tree[i];
                    if (rec is GDMIndividualRecord) {
                        cnt++;
                        GDMIndividualRecord iRec = rec as GDMIndividualRecord;
                        string st = iRec.XRef + " / " + GKUtils.GetNameString(iRec, true, false);

                        if (fSplitList.IndexOf(iRec) < 0) {
                            fView.SkippedList.AddItem(null, st);
                        } else {
                            fView.SelectedList.AddItem(null, st);
                        }
                    }
                }
                fView.Title = fSplitList.Count.ToString() + @" / " + cnt.ToString();
            } finally {
                fView.SelectedList.EndUpdate();
                fView.SkippedList.EndUpdate();
            }
        }

        public void Select(TreeTools.TreeWalkMode walkMode)
        {
            Select(fBase.GetSelectedPerson(), walkMode);
        }

        public void Select(GDMIndividualRecord startPerson, TreeTools.TreeWalkMode walkMode)
        {
            fSplitList.Clear();

            if (startPerson == null) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
            } else {
                TreeTools.WalkTree(fBase.Context.Tree, startPerson, walkMode, fSplitList);
            }

            UpdateView();
        }

        public void Delete()
        {
            int num = fSplitList.Count;
            if (num == 0) return;

            for (int i = 0; i < num; i++) {
                object obj = fSplitList[i];

                if (obj is GDMIndividualRecord) {
                    BaseController.DeleteRecord(fBase, obj as GDMIndividualRecord, false);
                }
            }

            fSplitList.Clear();
            UpdateView();
            fBase.RefreshLists(false);

            AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.LSID_RecsDeleted));
        }

        public void Save()
        {
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT, "");
            if (string.IsNullOrEmpty(fileName)) return;

            TreeTools.CheckRelations(fBase.Context.Tree, fSplitList);

            var tree = fBase.Context.Tree;
            GKUtils.PrepareHeader(tree, fileName, GlobalOptions.Instance.DefCharacterSet, true);

            using (StreamWriter fs = new StreamWriter(fileName, false, GEDCOMUtils.GetEncodingByCharacterSet(tree.Header.CharacterSet.Value))) {
                var gedcomProvider = new GEDCOMProvider(tree);
                gedcomProvider.SaveToStream(fs, fSplitList);
            }
        }
    }
}
