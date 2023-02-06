/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Design.MVP.Controls;
using GKCore.MVP;
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
            List<List<GDMRecord>> treeFragments = null;

            AppHost.Instance.ExecuteWork((controller) => {
                treeFragments = TreeTools.SearchTreeFragments(fBase.Context.Tree, controller);
            });

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

                        string pn = GKUtils.GetNameString(iRec, true, false);
                        if (iRec.Patriarch) {
                            pn = "(*) " + pn;
                        }
                        pn = string.Join(" ", pn, "[", iRec.XRef, "]");

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

        public GDMIndividualRecord GetSelectedPerson()
        {
            return fView.GroupsTree.GetSelectedData() as GDMIndividualRecord;
        }

        public void SelectPerson()
        {
            GDMIndividualRecord iRec = GetSelectedPerson();
            if (iRec == null) return;

            fBase.SelectRecordByXRef(iRec.XRef);
            fBase.Activate();
        }

        public void ShowDetails()
        {
            GDMIndividualRecord iRec = GetSelectedPerson();
            if (iRec == null) return;

            BaseController.ViewRecordInfo(fBase, iRec);
        }

        public void CopySelectedXRef()
        {
            var rec = GetSelectedPerson();
            if (rec != null)
                AppHost.Instance.SetClipboardText(rec.XRef);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_ToolOp_6);

            GetControl<ITabPage>("pageFamilyGroups").Text = LangMan.LS(LSID.LSID_ToolOp_6);
            GetControl<IButton>("btnAnalyseGroups").Text = LangMan.LS(LSID.LSID_Analyze);
            GetControl<IMenuItem>("miDetails").Text = LangMan.LS(LSID.LSID_Details);
            GetControl<IMenuItem>("miGoToRecord").Text = LangMan.LS(LSID.LSID_GoToPersonRecord);
            GetControl<IMenuItem>("miCopyXRef").Text = LangMan.LS(LSID.LSID_CopyXRef);
        }

        public void OpeningContextMenu()
        {
            var iRec = GetSelectedPerson();
            GetControl<IMenuItem>("miDetails").Enabled = (iRec != null);
            GetControl<IMenuItem>("miGoToRecord").Enabled = (iRec != null);
            GetControl<IMenuItem>("miCopyXRef").Enabled = (iRec != null);
        }
    }
}
