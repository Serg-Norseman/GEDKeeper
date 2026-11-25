/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using BSLib.DataViz.Model;
using BSLib.DataViz.TreeMap;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Filters;
using GKCore.Locales;
using GKCore.Tools;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class FragmentSearchController : DialogController<IFragmentSearchDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabPage _pageFamilyGroups;
        private ITreeView _tvGroups;
        private ILogChart _gkLogChart1;
        private IButton _btnAnalyseGroups;
        private IMenuItem _miDetails;
        private IMenuItem _miGoToRecord;
        private IMenuItem _miCopyXRef;
        private ITabPage _pageDataQuality;
        private IMenuItem _miDQRefresh;
        private IMenuItem _miDQResetFilter;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        private bool fTerm;

        public FragmentSearchController(IFragmentSearchDlg view) : base(view)
        {
            fTerm = !AppHost.Instance.HasFeatureSupport(Feature.Graphics);
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

            _gkLogChart1.Clear();

            _tvGroups.BeginUpdate();
            try {
                _tvGroups.Clear();

                int num = treeFragments.Count;
                for (int i = 0; i < num; i++) {
                    var groupRecords = treeFragments[i];

                    int cnt = groupRecords.Count;
                    int groupNum = (i + 1);
                    ITVNode groupItem = _tvGroups.AddNode(null,
                        groupNum.ToString() + " " + LangMan.LS(LSID.Group).ToLower() + " (" + cnt.ToString() + ")", null);

                    for (int j = 0; j < cnt; j++) {
                        var iRec = (GDMIndividualRecord)groupRecords[j];

                        string pn = GKUtils.GetNameString(iRec, true, false);
                        if (iRec.Patriarch) {
                            pn = "(*) " + pn;
                        }
                        pn = string.Join(" ", pn, "[", iRec.XRef, "]");

                        _tvGroups.AddNode(groupItem, pn, iRec);
                    }

                    _tvGroups.Expand(groupItem);

                    _gkLogChart1.AddFragment(cnt);
                }
            } finally {
                treeFragments.Clear();
                _tvGroups.EndUpdate();
            }
        }

        public GDMIndividualRecord GetSelectedPerson()
        {
            return _tvGroups.GetSelectedData() as GDMIndividualRecord;
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
            fView.SetTitle(LangMan.LS(LSID.FragmentSearch));

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                _miDetails.Text = LangMan.LS(LSID.Details);
                _miGoToRecord.Text = LangMan.LS(LSID.GoToPersonRecord);
                _miCopyXRef.Text = LangMan.LS(LSID.CopyXRef);

                if (AppHost.Instance.HasFeatureSupport(Feature.Graphics)) {
                    _miDQRefresh.Text = LangMan.LS(LSID.Refresh);
                    _miDQResetFilter.Text = LangMan.LS(LSID.ResetFilter);
                }
            }

            _btnAnalyseGroups.Text = LangMan.LS(LSID.Analyze);

            if (AppHost.Instance.HasFeatureSupport(Feature.Graphics)) {
                _pageFamilyGroups.Text = LangMan.LS(LSID.FragmentSearch);
                _pageDataQuality.Text = LangMan.LS(LSID.DataQuality);
            }
        }

        public override void ApplyTheme()
        {
            // dummy
        }

        public void OpeningContextMenu()
        {
            var iRec = GetSelectedPerson();
            _miDetails.Enabled = (iRec != null);
            _miGoToRecord.Enabled = (iRec != null);
            _miCopyXRef.Enabled = (iRec != null);
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

                            TreeTools.WalkTree(tree, iRec, TreeWalkMode.twmAll, groupRecords);

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
