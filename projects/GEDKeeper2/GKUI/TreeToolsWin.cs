/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Components;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TreeToolsWin : Form
    {
        public enum ToolType { ttTreeCompare, ttTreeMerge, ttTreeSplit, ttRecMerge, ttFamilyGroups, ttTreeCheck, ttPatSearch, ttPlaceManage }

        // runtime
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;

        private readonly List<GEDCOMRecord> fSplitList;
        private GEDCOMRecordType fRMMode;
        private readonly StringList fRMSkip;
        private int fRMIndex;
        private readonly StringList fPlaces;
        private readonly List<TreeTools.CheckObj> fChecksList;

        // UI
        private GKListView ListPlaces;
        private GKListView ListChecks;
        private GKListView ListPatriarchs;


        public IBaseWindow Base
        {
            get { return fBase; }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //fChecksList.Dispose();
                TreeTools.PlacesSearch_Clear(fPlaces);
                fPlaces.Dispose();
                fRMSkip.Dispose();
                //fSplitList.Dispose();
            }
            base.Dispose(disposing);
        }

        public TreeToolsWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = GKResources.iBtnCancel;

            fBase = baseWin;
            fTree = Base.Context.Tree;

            tabsTools.SelectedIndex = 0;

            fSplitList = new List<GEDCOMRecord>();
            fRMSkip = new StringList();
            fRMMode = GEDCOMRecordType.rtIndividual;

            MergeCtl.Base = fBase;
            MergeCtl.MergeMode = fRMMode;

            fPlaces = new StringList();
            fPlaces.Sorted = true;
            fChecksList = new List<TreeTools.CheckObj>();
            gkLogChart1.OnHintRequest += HintRequestEventHandler;

            PrepareChecksList();
            PreparePatriarchsList();
            PreparePlacesList();

            SetLang();
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MITreeTools);

            pageTreeCompare.Text = LangMan.LS(LSID.LSID_ToolOp_1);
            pageTreeMerge.Text = LangMan.LS(LSID.LSID_ToolOp_2);
            pageTreeSplit.Text = LangMan.LS(LSID.LSID_ToolOp_3);
            pageRecMerge.Text = LangMan.LS(LSID.LSID_ToolOp_4);
            pageFamilyGroups.Text = LangMan.LS(LSID.LSID_ToolOp_6);
            pageTreeCheck.Text = LangMan.LS(LSID.LSID_ToolOp_7);
            pagePatSearch.Text = LangMan.LS(LSID.LSID_ToolOp_8);
            pagePlaceManage.Text = LangMan.LS(LSID.LSID_ToolOp_9);

            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            lblFile.Text = LangMan.LS(LSID.LSID_MIFile);

            btnFileChoose.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            btnTreeMerge.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";

            btnSelectAll.Text = LangMan.LS(LSID.LSID_SelAll);
            btnSelectFamily.Text = LangMan.LS(LSID.LSID_SelFamily);
            btnSelectAncestors.Text = LangMan.LS(LSID.LSID_SelAncestors);
            btnSelectDescendants.Text = LangMan.LS(LSID.LSID_SelDescendants);
            btnDelete.Text = LangMan.LS(LSID.LSID_DoDelete);
            btnSave.Text = LangMan.LS(LSID.LSID_MIFileSaveAs);
            pageMerge.Text = LangMan.LS(LSID.LSID_RecMerge);
            pageMergeOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnAutoSearch.Text = LangMan.LS(LSID.LSID_RM_Search);
            btnSkip.Text = LangMan.LS(LSID.LSID_RM_Skip);
            rgMode.Text = LangMan.LS(LSID.LSID_RM_Records);
            radPersons.Text = LangMan.LS(LSID.LSID_RPIndividuals);
            radNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            radFamilies.Text = LangMan.LS(LSID.LSID_RPFamilies);
            radSources.Text = LangMan.LS(LSID.LSID_RPSources);
            grpSearchPersons.Text = LangMan.LS(LSID.LSID_RM_SearchPersons);
            chkIndistinctMatching.Text = LangMan.LS(LSID.LSID_RM_IndistinctMatching);
            chkBirthYear.Text = LangMan.LS(LSID.LSID_RM_BirthYear);
            lblNameAccuracy.Text = LangMan.LS(LSID.LSID_RM_NameAccuracy);
            lblYearInaccuracy.Text = LangMan.LS(LSID.LSID_RM_YearInaccuracy);
            btnBaseRepair.Text = LangMan.LS(LSID.LSID_Repair);
            lblMinGenerations.Text = LangMan.LS(LSID.LSID_MinGenerations);
            btnSetPatriarch.Text = LangMan.LS(LSID.LSID_SetPatFlag);
            btnPatSearch.Text = LangMan.LS(LSID.LSID_Search);
            btnIntoList.Text = LangMan.LS(LSID.LSID_InsertIntoBook);

            grpMergeOther.Text = LangMan.LS(LSID.LSID_Other);
            chkBookmarkMerged.Text = LangMan.LS(LSID.LSID_BookmarkMerged);

            lblMasterBase.Text = LangMan.LS(LSID.LSID_MasterBase);
            lblOtherBase.Text = LangMan.LS(LSID.LSID_OtherBase);
            edMasterBase.Text = LangMan.LS(LSID.LSID_CurrentBase);

            grpMatchType.Text = LangMan.LS(LSID.LSID_MatchType);
            radMatchInternal.Text = LangMan.LS(LSID.LSID_MatchInternal);
            radMathExternal.Text = LangMan.LS(LSID.LSID_MathExternal);
            radAnalysis.Text = LangMan.LS(LSID.LSID_Analysis);
            btnMatch.Text = LangMan.LS(LSID.LSID_Match);
            chkWithoutDates.Text = LangMan.LS(LSID.LSID_WithoutDates);
            btnPatriarchsDiagram.Text = LangMan.LS(LSID.LSID_PatriarchsDiagram);
        }

        private void tabsTools_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (tabsTools.SelectedTab == pageFamilyGroups)
            {
                CheckGroups();
            }
            else if (tabsTools.SelectedTab == pageTreeCheck)
            {
                CheckBase();
            }
            else if (tabsTools.SelectedTab == pagePlaceManage)
            {
                CheckPlaces();
            }
        }

        #region TreeMerge

        private void btnTreeMerge_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            edUpdateBase.Text = fileName;
            TreeTools.TreeMerge(Base.Context.Tree, edUpdateBase.Text, mSyncRes);
            Base.RefreshLists(false);
        }

        #endregion

        #region Duplicates Search

        private void SheetMergeResize(object sender, EventArgs e)
        {
            //MergeCtl.Height = SheetMerge.Height - btnSearch.Top - 20;
        }

        private static bool CheckPersonsEx(GEDCOMIndividualRecord rec1, GEDCOMIndividualRecord rec2)
        {
            GEDCOMFamilyRecord fam1 = rec1.GetParentsFamily();
            GEDCOMFamilyRecord fam2 = rec2.GetParentsFamily();

            return (!Equals(fam1, fam2));
        }

        private void SearchDups()
        {
            MergeCtl.Base = fBase;
            MergeCtl.MergeMode = fRMMode;
            
            MergeCtl.SetRec1(null);
            MergeCtl.SetRec2(null);
            
            MatchParams mParams;
            //mParams.IndistinctNameMatching = chkIndistinctMatching.Checked;
            mParams.NamesIndistinctThreshold = (float)decimal.ToDouble(edNameAccuracy.Value) / 100.0f;
            mParams.DatesCheck = chkBirthYear.Checked;
            mParams.YearsInaccuracy = decimal.ToInt32(edYearInaccuracy.Value);

            bool res = false;
            btnSkip.Enabled = false;

            try
            {
                ProgressBar1.Minimum = 0;
                ProgressBar1.Maximum = fTree.RecordsCount;
                ProgressBar1.Value = fRMIndex;

                int recNum = fTree.RecordsCount;
                for (int i = fRMIndex; i < recNum; i++)
                {
                    fRMIndex = i;
                    ProgressBar1.Increment(1);

                    GEDCOMRecord iRec = fTree[i];
                    if (iRec.RecordType != fRMMode) continue;

                    for (int j = i + 1; j < recNum; j++)
                    {
                        GEDCOMRecord kRec = fTree[j];
                        if (kRec.RecordType != fRMMode) continue;

                        if (iRec == kRec) continue;
                        if (fRMSkip.IndexOf(iRec.XRef + "-" + kRec.XRef) >= 0) continue;

                        res = iRec.IsMatch(kRec, mParams) >= 100.0f;

                        if (res && fRMMode == GEDCOMRecordType.rtIndividual)
                        {
                            res = CheckPersonsEx((GEDCOMIndividualRecord)iRec, (GEDCOMIndividualRecord)kRec);
                        }

                        if (res)
                        {
                            MergeCtl.SetRec1(iRec);
                            MergeCtl.SetRec2(kRec);
                            break;
                        }
                    }

                    if (res) break;
                }
            }
            finally
            {
                btnSkip.Enabled = true;
            }
        }

        private void radMergeMode_Click(object sender, EventArgs e)
        {
            if (radPersons.Checked) fRMMode = GEDCOMRecordType.rtIndividual;
            if (radNotes.Checked) fRMMode = GEDCOMRecordType.rtNote;
            if (radFamilies.Checked) fRMMode = GEDCOMRecordType.rtFamily;
            if (radSources.Checked) fRMMode = GEDCOMRecordType.rtSource;

            MergeCtl.MergeMode = fRMMode;
        }

        private void chkBookmarkMerged_CheckedChanged(object sender, EventArgs e)
        {
            MergeCtl.Bookmark = chkBookmarkMerged.Checked;
        }

        private void btnSkip_Click(object sender, EventArgs e)
        {
            if (MergeCtl.Rec1 != null && MergeCtl.Rec2 != null)
            {
                fRMSkip.Add(MergeCtl.Rec1.XRef + "-" + MergeCtl.Rec2.XRef);
            }
            SearchDups();
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            fRMIndex = 0;
            fRMSkip.Clear();
            SearchDups();
        }

        #endregion

        #region CheckGroups

        private void CheckGroups()
        {
            IProgressController progress = AppHost.Progress;

            gkLogChart1.Clear();
            progress.ProgressInit(LangMan.LS(LSID.LSID_CheckFamiliesConnection), fTree.RecordsCount);
            List<GEDCOMIndividualRecord> prepared = new List<GEDCOMIndividualRecord>();
            List<GEDCOMRecord> groupRecords = new List<GEDCOMRecord>();
            try
            {
                int groupNum = 0;
                tvGroups.Nodes.Clear();

                int num = fTree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = fTree[i];

                    if (rec is GEDCOMIndividualRecord)
                    {
                        GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
                        if (prepared.IndexOf(iRec) < 0)
                        {
                            groupNum++;
                            groupRecords.Clear();

                            TreeTools.TreeWalk(iRec, TreeTools.TreeWalkMode.twmAll, groupRecords);

                            int cnt = groupRecords.Count;

                            TreeNode root = tvGroups.Nodes.Add(
                                groupNum.ToString() + " " + LangMan.LS(LSID.LSID_Group).ToLower() + " (" + cnt.ToString() + ")");

                            for (int j = 0; j < cnt; j++)
                            {
                                iRec = (GEDCOMIndividualRecord)groupRecords[j];
                                prepared.Add(iRec);

                                string pn = GKUtils.GetNameString(iRec, true, false);
                                if (iRec.Patriarch)
                                {
                                    pn = "(*) " + pn;
                                }
                                root.Nodes.Add(new GKTreeNode(pn, iRec));
                            }
                            root.ExpandAll();

                            gkLogChart1.AddFragment(cnt);
                        }
                    }

                    progress.ProgressStep();
                    Application.DoEvents();
                }
            }
            finally
            {
                groupRecords.Clear();
                //prepared.Dispose();
                progress.ProgressDone();
            }
        }

        private void tvGroups_DoubleClick(object sender, EventArgs e)
        {
            GKTreeNode node = tvGroups.SelectedNode as GKTreeNode;
            if (node == null) return;
            
            GEDCOMIndividualRecord iRec = node.Tag as GEDCOMIndividualRecord;
            if (iRec == null) return;
            
            Base.SelectRecordByXRef(iRec.XRef);
            Close();
        }

        private void HintRequestEventHandler(object sender, HintRequestEventArgs args)
        {
            if (args == null) return;

            args.Hint = string.Format(LangMan.LS(LSID.LSID_LogHint), args.FragmentNumber, args.Size);
        }

        #endregion

        #region Tree Verify

        private void PrepareChecksList()
        {
            ListChecks = UIHelper.CreateListView(Panel1);
            ListChecks.CheckBoxes = true;
            ListChecks.DoubleClick += ListChecks_DblClick;
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Record), 400, false);
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Problem), 200, false);
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Solve), 200, false);
        }

        private void CheckBase()
        {
            TreeTools.CheckBase(fBase, fChecksList);

            ListChecks.Items.Clear();

            foreach (TreeTools.CheckObj checkObj in fChecksList)
            {
                ListChecks.AddItem(checkObj, new object[] { checkObj.GetRecordName(),
                                       checkObj.Comment,
                                       LangMan.LS(GKData.CheckSolveNames[(int)checkObj.Solve]) });
            }

            ListChecks.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
        }

        private void btnBaseRepair_Click(object sender, EventArgs e)
        {
            try
            {
                int num = ListChecks.Items.Count;
                for (int i = 0; i < num; i++)
                {
                    GKListItem item = (GKListItem)ListChecks.Items[i];
                    if (!item.Checked) continue;

                    TreeTools.CheckObj checkObj = item.Data as TreeTools.CheckObj;
                    TreeTools.RepairProblem(fBase, checkObj);
                }
            }
            finally
            {
                Base.RefreshLists(false);
                CheckBase();
            }
        }

        private void ListChecks_DblClick(object sender, EventArgs e)
        {
            GKListItem item = ListChecks.GetSelectedItem();
            if (item == null) return;

            GEDCOMIndividualRecord iRec = ((TreeTools.CheckObj)item.Data).Rec as GEDCOMIndividualRecord;
            if (iRec == null) return;

            Base.SelectRecordByXRef(iRec.XRef);
            Close();
        }

        #endregion

        #region Places Management

        private void PreparePlacesList()
        {
            ListPlaces = UIHelper.CreateListView(Panel4);
            ListPlaces.DoubleClick += ListPlaces_DblClick;
            ListPlaces.AddColumn(LangMan.LS(LSID.LSID_Place), 400, false);
            ListPlaces.AddColumn(LangMan.LS(LSID.LSID_LinksCount), 100, false);
        }

        private void CheckPlaces()
        {
            ListPlaces.BeginUpdate();
            try
            {
                TreeTools.PlacesSearch(fTree, fPlaces, AppHost.Progress);

                ListPlaces.Items.Clear();

                int num4 = fPlaces.Count;
                for (int i = 0; i < num4; i++)
                {
                    PlaceObj placeObj = (PlaceObj)fPlaces.GetObject(i);

                    ListPlaces.AddItem(placeObj, new object[] { fPlaces[i], placeObj.Facts.Count });
                }
            }
            finally
            {
                ListPlaces.EndUpdate();
            }
        }

        private void btnIntoList_Click(object sender, EventArgs e)
        {
            ListPlaces_DblClick(null, null);
        }

        private void ListPlaces_DblClick(object sender, EventArgs e)
        {
            GKListItem item = ListPlaces.GetSelectedItem();
            if (item == null) return;
            
            PlaceObj pObj = item.Data as PlaceObj;
            if (pObj == null) return;
            
            if (pObj.Name.IndexOf("[*]") == 0)
            {
                AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.LSID_PlaceAlreadyInBook));
            }
            else
            {
                GEDCOMLocationRecord loc = fBase.Context.SelectRecord(GEDCOMRecordType.rtLocation, new object[] { pObj.Name }) as GEDCOMLocationRecord;
                if (loc == null) return;

                int num = pObj.Facts.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMCustomEvent evt = pObj.Facts[i];
                    evt.Place.StringValue = loc.LocationName;
                    evt.Place.Location.Value = loc;
                }

                CheckPlaces();
                Base.RefreshLists(false);
            }
        }

        #endregion

        #region Tree Splitting

        private void UpdateSplitLists()
        {
            ListSelected.BeginUpdate();
            ListSelected.Items.Clear();
            ListSkipped.BeginUpdate();
            ListSkipped.Items.Clear();
            try
            {
                int cnt = 0;

                int num = fTree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = fTree[i];
                    if (rec is GEDCOMIndividualRecord)
                    {
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
            }
            finally
            {
                ListSelected.EndUpdate();
                ListSkipped.EndUpdate();
            }
        }

        private void Select(GEDCOMIndividualRecord startPerson, TreeTools.TreeWalkMode walkMode)
        {
            fSplitList.Clear();
            TreeTools.TreeWalk(startPerson, walkMode, fSplitList);
            UpdateSplitLists();
        }

        private void btnSelectFamily_Click(object sender, EventArgs e)
        {
            Select(Base.GetSelectedPerson(), TreeTools.TreeWalkMode.twmFamily);
        }

        private void btnSelectAncestors_Click(object sender, EventArgs e)
        {
            Select(Base.GetSelectedPerson(), TreeTools.TreeWalkMode.twmAncestors);
        }

        private void btnSelectDescendants_Click(object sender, EventArgs e)
        {
            Select(Base.GetSelectedPerson(), TreeTools.TreeWalkMode.twmDescendants);
        }

        private void btnSelectAll_Click(object sender, EventArgs e)
        {
            Select(Base.GetSelectedPerson(), TreeTools.TreeWalkMode.twmAll);
        }

        private void btnDelete_Click(object sender, EventArgs e)
        {
            int num = fSplitList.Count;
            if (num == 0) return;

            for (int i = 0; i < num; i++) {
                object obj = fSplitList[i];

                if (obj is GEDCOMIndividualRecord) {
                    BaseController.DeleteRecord(Base, obj as GEDCOMIndividualRecord, false);
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

            using (StreamWriter fs = new StreamWriter(fileName, false, GEDCOMUtils.GetEncodingByCharacterSet(fTree.Header.CharacterSet)))
            {
                var gedcomProvider = new GEDCOMProvider(fTree);
                gedcomProvider.SaveToStream(fs, fSplitList);

                fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
            }
        }

        #endregion

        #region Patriarchs Search

        private void PreparePatriarchsList()
        {
            ListPatriarchs = UIHelper.CreateListView(Panel3);
            ListPatriarchs.DoubleClick += ListPatriarchs_DblClick;
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Patriarch), 400, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Birth), 90, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Descendants), 90, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Generations), 90, false);
        }

        private void ListPatriarchs_DblClick(object sender, EventArgs e)
        {
            GKListItem item = ListPatriarchs.GetSelectedItem();
            if (item == null) return;

            GEDCOMIndividualRecord iRec = item.Data as GEDCOMIndividualRecord;
            if (iRec == null) return;

            Base.SelectRecordByXRef(iRec.XRef);
            Close();
        }

        private static int PatriarchsCompare(object item1, object item2)
        {
            return ((PatriarchObj)item1).BirthYear - ((PatriarchObj)item2).BirthYear;
        }

        private void btnPatSearch_Click(object sender, EventArgs e)
        {
            ListPatriarchs.BeginUpdate();
            ExtList<PatriarchObj> lst = null;
            try
            {
                ListPatriarchs.Items.Clear();
                lst = PatriarchsMan.GetPatriarchsList(fBase.Context,
                                                      decimal.ToInt32(edMinGens.Value), !chkWithoutDates.Checked);
                lst.QuickSort(PatriarchsCompare);

                int num = lst.Count;
                for (int i = 0; i < num; i++)
                {
                    PatriarchObj pObj = lst[i];
                    string pSign = ((pObj.IRec.Patriarch) ? "[*] " : "");

                    ListPatriarchs.AddItem(pObj.IRec, new object[] { pSign + GKUtils.GetNameString(pObj.IRec, true, false),
                                               pObj.BirthYear, pObj.DescendantsCount, pObj.DescGenerations });
                }
            }
            finally
            {
                if (lst != null) lst.Dispose();
                ListPatriarchs.EndUpdate();
            }
        }

        private void btnSetPatriarch_Click(object sender, EventArgs e)
        {
            try
            {
                GKListItem item = ListPatriarchs.GetSelectedItem();
                if (item == null) return;

                GEDCOMIndividualRecord iRec = item.Data as GEDCOMIndividualRecord;
                if (iRec != null)
                {
                    iRec.Patriarch = true;
                }

                Base.RefreshLists(false);
            }
            finally
            {
                btnPatSearch_Click(null, null);
            }
        }

        private void btnPatriarchsDiagram_Click(object sender, EventArgs e)
        {
            PatriarchsViewerWin wnd = new PatriarchsViewerWin(fBase, decimal.ToInt32(edMinGens.Value));
            wnd.Show();
        }

        #endregion

        #region Match files

        private string external_match_db;
        private enum TreeMatchType { tmtInternal, tmtExternal, tmtAnalysis }

        private void btnFileChoose_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            external_match_db = fileName;
            txtCompareFile.Text = Path.GetFileName(external_match_db);
        }

        private void DuplicateFoundFunc(GEDCOMIndividualRecord indivA, GEDCOMIndividualRecord indivB)
        {
            ListCompare.AppendText("    * [" + GKUtils.GetNameString(indivA, true, false) + "]\r\n");
            ListCompare.AppendText("      [" + GKUtils.GetNameString(indivB, true, false) + "]\r\n\r\n");
            //ListCompare.AppendText("\r\n");
        }

        private TreeMatchType GetTreeMatchType()
        {
            TreeMatchType type =
                ((radMatchInternal.Checked) ?
                 TreeMatchType.tmtInternal :
                 ((radMathExternal.Checked) ? TreeMatchType.tmtExternal : TreeMatchType.tmtAnalysis));

            return type;
        }

        private void btnMatch_Click(object sender, EventArgs e)
        {
            TreeMatchType type = GetTreeMatchType();

            ListCompare.Clear();

            switch (type) {
                case TreeMatchType.tmtInternal:
                    TreeTools.FindDuplicates(fTree, fTree, 90 /*min: 80-85*/, DuplicateFoundFunc, AppHost.Progress);
                    break;

                case TreeMatchType.tmtExternal:
                    TreeTools.TreeCompare(fBase.Context, external_match_db, ListCompare);
                    break;

                case TreeMatchType.tmtAnalysis:
                    {
                        List<TreeTools.ULIndividual> uln = TreeTools.GetUnlinkedNamesakes(fBase);

                        ListCompare.AppendText("  " + LangMan.LS(LSID.LSID_SearchUnlinkedNamesakes) + ":\r\n");
                        if (uln != null && uln.Count > 0)
                        {
                            int num = uln.Count;
                            foreach (TreeTools.ULIndividual indiv in uln)
                            {
                                ListCompare.AppendText("    - [" + indiv.Family + "] " + GKUtils.GetNameString(indiv.IRec, true, false) + "\r\n");
                            }
                        }
                        else
                        {
                            ListCompare.AppendText("    - not found.");
                        }
                        break;
                    }
            }
        }

        private void rbtnMatch_CheckedChanged(object sender, EventArgs e)
        {
            TreeMatchType type = GetTreeMatchType();

            lblFile.Enabled = (type == TreeMatchType.tmtExternal);
            txtCompareFile.Enabled = (type == TreeMatchType.tmtExternal);
            btnFileChoose.Enabled = (type == TreeMatchType.tmtExternal);
        }

        #endregion
    }
}
