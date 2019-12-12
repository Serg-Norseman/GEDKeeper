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

using System.Collections.Generic;
using GDModel;
using GKCore.Interfaces;
using GKCore.MVP;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Tools;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class FragmentSearchController : DialogController<IFragmentSearchDlg>
    {
        public FragmentSearchController(IFragmentSearchDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
        }

        public void CheckGroups()
        {
            IProgressController progress = AppHost.Progress;
            List<List<GDMRecord>> treeFragments = TreeTools.SearchTreeFragments(fBase.Context.Tree, progress);

            fView.LogChart.Clear();
            fView.GroupsTree.BeginUpdate();
            try {
                fView.GroupsTree.Clear();

                int num = treeFragments.Count;
                for (int i = 0; i < num; i++) {
                    var groupRecords = treeFragments[i];

                    int cnt = groupRecords.Count;
                    int groupNum = (i + 1);
                    ITVNode groupItem = fView.GroupsTree.AddNode(null,
                        groupNum.ToString() + " " + LangMan.LS(LSID.LSID_Group).ToLower() + " (" + cnt.ToString() + ")", null);

                    for (int j = 0; j < cnt; j++) {
                        var iRec = (GDMIndividualRecord)groupRecords[j];

                        string pn = iRec.GetNameString(true, false);
                        if (iRec.Patriarch) {
                            pn = "(*) " + pn;
                        }
                        fView.GroupsTree.AddNode(groupItem, pn, iRec);
                    }

                    fView.GroupsTree.Expand(groupItem);
                    fView.LogChart.AddFragment(cnt);
                }
            } finally {
                treeFragments.Clear();
                fView.GroupsTree.EndUpdate();
            }
        }

        public void SelectPerson()
        {
            GDMIndividualRecord iRec = fView.GroupsTree.GetSelectedData() as GDMIndividualRecord;
            if (iRec == null) return;

            fBase.SelectRecordByXRef(iRec.XRef);
            fView.Close();
        }
    }
}
