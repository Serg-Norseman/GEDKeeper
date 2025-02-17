/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using BSLib.DataViz.Model;
using BSLib.DataViz.TreeMap;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class FragmentSearchController : DialogController<IFragmentSearchDlg>
    {
        public const double ColdWavelength = 520.0;

        public class GroupMapItem : MapItem
        {
            public int Color;
            public List<GDMRecord> GroupRecords;

            public GroupMapItem(MapItem parent, string name, double size) : base(parent, name, size)
            {
            }
        }

        private GroupMapItem fCurrentItem;

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
                        groupNum.ToString() + " " + LangMan.LS(LSID.Group).ToLower() + " (" + cnt.ToString() + ")", null);

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

            BaseController.ViewRecordInfo(fView, fBase, iRec);
        }

        public void CopySelectedXRef()
        {
            var rec = GetSelectedPerson();
            if (rec != null)
                AppHost.Instance.SetClipboardText(rec.XRef);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.FragmentSearch);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<IMenuItem>("miDetails").Text = LangMan.LS(LSID.Details);
                GetControl<IMenuItem>("miGoToRecord").Text = LangMan.LS(LSID.GoToPersonRecord);
                GetControl<IMenuItem>("miCopyXRef").Text = LangMan.LS(LSID.CopyXRef);
                GetControl<IMenuItem>("miDQRefresh").Text = LangMan.LS(LSID.Refresh);
                GetControl<IMenuItem>("miDQResetFilter").Text = LangMan.LS(LSID.ResetFilter);
            }

            GetControl<ITabPage>("pageFamilyGroups").Text = LangMan.LS(LSID.FragmentSearch);
            GetControl<IButton>("btnAnalyseGroups").Text = LangMan.LS(LSID.Analyze);

            GetControl<ITabPage>("pageDataQuality").Text = LangMan.LS(LSID.DataQuality);
        }

        public override void ApplyTheme()
        {
            // dummy
        }

        public void OpeningContextMenu()
        {
            var iRec = GetSelectedPerson();
            GetControl<IMenuItem>("miDetails").Enabled = (iRec != null);
            GetControl<IMenuItem>("miGoToRecord").Enabled = (iRec != null);
            GetControl<IMenuItem>("miCopyXRef").Enabled = (iRec != null);
        }

        public void SetExternalFilter(ExternalFilterHandler handler)
        {
            if (fBase != null) {
                var listMan = fBase.GetRecordsListManByType(GDMRecordType.rtIndividual);
                listMan.ExternalFilter = handler;
                fBase.ApplyFilter(GDMRecordType.rtIndividual);
            }
        }

        public bool GroupFilterHandler(GDMRecord record)
        {
            return (fCurrentItem != null) && fCurrentItem.GroupRecords.Contains(record);
        }

        public MapItem DataMap_CreateGroupItem(MapItem parent, string name, double size)
        {
            return new GroupMapItem(parent, name, size);
        }

        public void DoubleClickGroupItem(TreemapModel treemapModel, int mX, int mY)
        {
            fCurrentItem = treemapModel.FindByCoord(mX, mY) as GroupMapItem;
            if (fCurrentItem != null) {
                SetExternalFilter(GroupFilterHandler);
            }
        }

        public void UpdateTreeMap(TreemapModel treemapModel)
        {
            treemapModel.Items.Clear();

            string hint = LangMan.LS(LSID.DQHint);

            GDMTree tree = fBase.Context.Tree;
            List<GDMIndividualRecord> prepared = new List<GDMIndividualRecord>();
            try {
                int groupNum = 0;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = tree[i];

                    if (rec.RecordType == GDMRecordType.rtIndividual) {
                        GDMIndividualRecord iRec = (GDMIndividualRecord)rec;
                        if (!prepared.Contains(iRec)) {
                            groupNum++;
                            var groupRecords = new List<GDMRecord>();

                            TreeTools.WalkTree(tree, iRec, TreeTools.TreeWalkMode.twmAll, groupRecords);

                            int groupSize = groupRecords.Count;
                            float quality = 0.0f;
                            for (int j = 0; j < groupSize; j++) {
                                iRec = (GDMIndividualRecord)groupRecords[j];
                                prepared.Add(iRec);

                                quality += GKUtils.GetCertaintyAssessment(iRec);
                            }
                            quality /= groupSize;

                            string name = string.Format(hint, groupNum, groupSize, quality.ToString("0.00"));

                            CreateItem(treemapModel, null, name, groupSize, quality, groupRecords);
                        }
                    }
                }
            } finally {
            }
        }

        private GroupMapItem CreateItem(TreemapModel treemapModel, MapItem parent, string name, double size, float quality, List<GDMRecord> groupRecords)
        {
            var item = treemapModel.CreateItem(parent, name, size) as GroupMapItem;
            item.GroupRecords = groupRecords;

            double wavelength = ColdWavelength + (Spectrum.WavelengthMaximum - ColdWavelength) * (1.0f - quality);
            item.Color = Spectrum.WavelengthToRGB(wavelength);

            return item;
        }
    }
}
