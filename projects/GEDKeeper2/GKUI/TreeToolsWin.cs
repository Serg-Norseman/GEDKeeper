/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Controls;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TreeToolsWin : Form
    {
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
            get	{ return this.fBase; }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //this.fChecksList.Dispose();
                TreeTools.PlacesSearch_Clear(this.fPlaces);
                this.fPlaces.Dispose();
                this.fRMSkip.Dispose();
                //this.fSplitList.Dispose();
            }
            base.Dispose(disposing);
        }

        public TreeToolsWin(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.btnClose.Image = (Image)MainWin.ResourceManager.GetObjectEx("iBtnCancel");

            this.fBase = aBase;
            this.fTree = this.Base.Tree;

            this.tabsTools.SelectedIndex = 0;

            this.fSplitList = new List<GEDCOMRecord>();
            this.fRMSkip = new StringList();
            this.fRMMode = GEDCOMRecordType.rtIndividual;

            this.MergeCtl.Base = this.fBase;
            this.MergeCtl.MergeMode = this.fRMMode;

            this.fPlaces = new StringList();
            this.fPlaces.Sorted = true;
            this.fChecksList = new List<TreeTools.CheckObj>();

            this.PrepareChecksList();
            this.PreparePatriarchsList();
            this.PreparePlacesList();

            this.SetLang();
        }

        public void SetLang()
        {
            this.Text = LangMan.LS(LSID.LSID_MITreeTools);

            this.pageTreeCompare.Text = LangMan.LS(LSID.LSID_ToolOp_1);
            this.pageTreeMerge.Text = LangMan.LS(LSID.LSID_ToolOp_2);
            this.pageTreeSplit.Text = LangMan.LS(LSID.LSID_ToolOp_3);
            this.pageRecMerge.Text = LangMan.LS(LSID.LSID_ToolOp_4);
            this.pageFamilyGroups.Text = LangMan.LS(LSID.LSID_ToolOp_6);
            this.pageTreeCheck.Text = LangMan.LS(LSID.LSID_ToolOp_7);
            this.pagePatSearch.Text = LangMan.LS(LSID.LSID_ToolOp_8);
            this.pagePlaceManage.Text = LangMan.LS(LSID.LSID_ToolOp_9);

            this.btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            this.lblFile.Text = LangMan.LS(LSID.LSID_MIFile);

            this.btnFileChoose.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            this.btnTreeMerge.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";

            this.btnSelectAll.Text = LangMan.LS(LSID.LSID_SelAll);
            this.btnSelectFamily.Text = LangMan.LS(LSID.LSID_SelFamily);
            this.btnSelectAncestors.Text = LangMan.LS(LSID.LSID_SelAncestors);
            this.btnSelectDescendants.Text = LangMan.LS(LSID.LSID_SelDescendants);
            this.btnDelete.Text = LangMan.LS(LSID.LSID_DoDelete);
            this.btnSave.Text = LangMan.LS(LSID.LSID_MIFileSave);
            this.pageMerge.Text = LangMan.LS(LSID.LSID_RecMerge);
            this.pageMergeOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            this.btnAutoSearch.Text = LangMan.LS(LSID.LSID_RM_Search);
            this.btnSkip.Text = LangMan.LS(LSID.LSID_RM_Skip);
            this.rgMode.Text = LangMan.LS(LSID.LSID_RM_Records);
            this.radPersons.Text = LangMan.LS(LSID.LSID_RPIndividuals);
            this.radNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.radFamilies.Text = LangMan.LS(LSID.LSID_RPFamilies);
            this.radSources.Text = LangMan.LS(LSID.LSID_RPSources);
            this.grpSearchPersons.Text = LangMan.LS(LSID.LSID_RM_SearchPersons);
            this.chkIndistinctMatching.Text = LangMan.LS(LSID.LSID_RM_IndistinctMatching);
            this.chkBirthYear.Text = LangMan.LS(LSID.LSID_RM_BirthYear);
            this.lblNameAccuracy.Text = LangMan.LS(LSID.LSID_RM_NameAccuracy);
            this.lblYearInaccuracy.Text = LangMan.LS(LSID.LSID_RM_YearInaccuracy);
            this.btnBaseRepair.Text = LangMan.LS(LSID.LSID_Repair);
            this.lblMinGenerations.Text = LangMan.LS(LSID.LSID_MinGenerations);
            this.btnSetPatriarch.Text = LangMan.LS(LSID.LSID_SetPatFlag);
            this.btnPatSearch.Text = LangMan.LS(LSID.LSID_Search);
            this.btnIntoList.Text = LangMan.LS(LSID.LSID_InsertIntoBook);

            this.grpMergeOther.Text = LangMan.LS(LSID.LSID_Other);
            this.chkBookmarkMerged.Text = LangMan.LS(LSID.LSID_BookmarkMerged);

            this.lblMasterBase.Text = LangMan.LS(LSID.LSID_MasterBase);
            this.lblOtherBase.Text = LangMan.LS(LSID.LSID_OtherBase);
            this.edMasterBase.Text = LangMan.LS(LSID.LSID_CurrentBase);

            this.grpMatchType.Text = LangMan.LS(LSID.LSID_MatchType);
            this.radMatchInternal.Text = LangMan.LS(LSID.LSID_MatchInternal);
            this.radMathExternal.Text = LangMan.LS(LSID.LSID_MathExternal);
            this.radAnalysis.Text = LangMan.LS(LSID.LSID_Analysis);
            this.btnMatch.Text = LangMan.LS(LSID.LSID_Match);
            this.chkWithoutDates.Text = LangMan.LS(LSID.LSID_WithoutDates);
            this.btnPatriarchsDiagram.Text = LangMan.LS(LSID.LSID_PatriarchsDiagram);
        }

        private void PageControl_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (this.tabsTools.SelectedTab == this.pageFamilyGroups)
            {
                this.CheckGroups();
            }
            else if (this.tabsTools.SelectedTab == this.pageTreeCheck)
            {
                this.CheckBase();
            }
            else if (this.tabsTools.SelectedTab == this.pagePlaceManage)
            {
                this.CheckPlaces();
            }
        }

        #region TreeMerge

        private void btnTreeMerge_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetOpenFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName))
            {
                this.edUpdateBase.Text = fileName;
                TreeTools.TreeMerge(this.Base.Tree, this.edUpdateBase.Text, this.mSyncRes);
                this.Base.RefreshLists(false);
            }
        }

        #endregion

        #region Duplicates Search

        private void SheetMergeResize(object sender, EventArgs e)
        {
            //this.MergeCtl.Height = SheetMerge.Height - btnSearch.Top - 20;
        }

        private static bool CheckPersonsEx(GEDCOMIndividualRecord rec1, GEDCOMIndividualRecord rec2)
        {
            GEDCOMFamilyRecord fam1 = rec1.GetParentsFamily();
            GEDCOMFamilyRecord fam2 = rec2.GetParentsFamily();

            return (!Equals(fam1, fam2));
        }

        private void SearchDups()
        {
            this.MergeCtl.Base = this.fBase;
            this.MergeCtl.MergeMode = this.fRMMode;
            
            this.MergeCtl.SetRec1(null);
            this.MergeCtl.SetRec2(null);
            
            MatchParams mParams;
            //mParams.IndistinctNameMatching = this.chkIndistinctMatching.Checked;
            mParams.NamesIndistinctThreshold = (float)decimal.ToDouble(this.edNameAccuracy.Value) / 100.0f;
            mParams.DatesCheck = this.chkBirthYear.Checked;
            mParams.YearsInaccuracy = decimal.ToInt32(this.edYearInaccuracy.Value);

            bool res = false;
            this.btnSkip.Enabled = false;

            try
            {
                this.ProgressBar1.Minimum = 0;
                this.ProgressBar1.Maximum = this.fTree.RecordsCount;
                this.ProgressBar1.Value = this.fRMIndex;

                int recNum = this.fTree.RecordsCount;
                for (int i = this.fRMIndex; i < recNum; i++)
                {
                    this.fRMIndex = i;
                    this.ProgressBar1.Increment(1);

                    GEDCOMRecord iRec = this.fTree[i];
                    if (iRec.RecordType != this.fRMMode) continue;

                    for (int j = i + 1; j < recNum; j++)
                    {
                        GEDCOMRecord kRec = this.fTree[j];
                        if (kRec.RecordType != this.fRMMode) continue;

                        if (iRec == kRec) continue;
                        if (this.fRMSkip.IndexOf(iRec.XRef + "-" + kRec.XRef) >= 0) continue;

                        res = iRec.IsMatch(kRec, mParams) >= 100.0f;

                        if (res && this.fRMMode == GEDCOMRecordType.rtIndividual)
                        {
                            res = CheckPersonsEx((GEDCOMIndividualRecord)iRec, (GEDCOMIndividualRecord)kRec);
                        }

                        if (res)
                        {
                            this.MergeCtl.SetRec1(iRec);
                            this.MergeCtl.SetRec2(kRec);
                            break;
                        }
                    }

                    if (res) break;
                }
            }
            finally
            {
                this.btnSkip.Enabled = true;
            }
        }

        private void RadioButton8_Click(object sender, EventArgs e)
        {
            if (this.radPersons.Checked) this.fRMMode = GEDCOMRecordType.rtIndividual;
            if (this.radNotes.Checked) this.fRMMode = GEDCOMRecordType.rtNote;
            if (this.radFamilies.Checked) this.fRMMode = GEDCOMRecordType.rtFamily;
            if (this.radSources.Checked) this.fRMMode = GEDCOMRecordType.rtSource;

            this.MergeCtl.MergeMode = this.fRMMode;
        }

        private void chkBookmarkMerged_CheckedChanged(object sender, EventArgs e)
        {
            this.MergeCtl.Bookmark = chkBookmarkMerged.Checked;
        }

        private void btnSkip_Click(object sender, EventArgs e)
        {
            if (this.MergeCtl.Rec1 != null && this.MergeCtl.Rec2 != null)
            {
                this.fRMSkip.Add(this.MergeCtl.Rec1.XRef + "-" + this.MergeCtl.Rec2.XRef);
            }
            this.SearchDups();
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            this.fRMIndex = 0;
            this.fRMSkip.Clear();
            this.SearchDups();
        }

        #endregion

        #region CheckGroups

        private void CheckGroups()
        {
            gkLogChart1.Clear();
            fBase.ProgressInit(LangMan.LS(LSID.LSID_CheckFamiliesConnection), this.fTree.RecordsCount);
            List<GEDCOMIndividualRecord> prepared = new List<GEDCOMIndividualRecord>();
            List<GEDCOMRecord> groupRecords = new List<GEDCOMRecord>();
            try
            {
                int groupNum = 0;
                this.tvGroups.Nodes.Clear();

                int num = this.fTree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = this.fTree[i];

                    if (rec is GEDCOMIndividualRecord)
                    {
                        GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
                        if (prepared.IndexOf(iRec) < 0)
                        {
                            groupNum++;
                            groupRecords.Clear();

                            TreeTools.TreeWalk(iRec, TreeTools.TreeWalkMode.twmAll, groupRecords);

                            int cnt = groupRecords.Count;

                            TreeNode root = this.tvGroups.Nodes.Add(
                                groupNum.ToString() + " " + LangMan.LS(LSID.LSID_Group).ToLower() + " (" + cnt.ToString() + ")");

                            for (int j = 0; j < cnt; j++)
                            {
                                iRec = (GEDCOMIndividualRecord)groupRecords[j];
                                prepared.Add(iRec);

                                string pn = iRec.GetNameString(true, false);
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

                    fBase.ProgressStep();
                    Application.DoEvents();
                }
            }
            finally
            {
                groupRecords.Clear();
                //prepared.Dispose();
                fBase.ProgressDone();
            }
        }

        private void TreeView1_DoubleClick(object sender, EventArgs e)
        {
            GKTreeNode node = this.tvGroups.SelectedNode as GKTreeNode;
            if (node == null) return;
            
            GEDCOMIndividualRecord iRec = node.Tag as GEDCOMIndividualRecord;
            if (iRec == null) return;
            
            this.Base.SelectRecordByXRef(iRec.XRef);
            base.Close();
        }

        #endregion

        #region Tree Verify

        private void PrepareChecksList()
        {
            this.ListChecks = GKUtils.CreateListView(this.Panel1);
            this.ListChecks.CheckBoxes = true;
            this.ListChecks.DoubleClick += this.ListChecksDblClick;
            this.ListChecks.AddListColumn(LangMan.LS(LSID.LSID_Record), 400, false);
            this.ListChecks.AddListColumn(LangMan.LS(LSID.LSID_Problem), 200, false);
            this.ListChecks.AddListColumn(LangMan.LS(LSID.LSID_Solve), 200, false);
        }

        private void CheckBase()
        {
            TreeTools.CheckBase(this.fBase, this.fChecksList);

            this.ListChecks.Items.Clear();

            foreach (TreeTools.CheckObj checkObj in this.fChecksList)
            {
                GKListItem item = this.ListChecks.AddItem(checkObj.GetRecordName(), checkObj);
                item.AddSubItem(checkObj.Comment);
                item.AddSubItem(LangMan.LS(GKData.CheckSolveNames[(int)checkObj.Solve]));
            }

            this.ListChecks.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
        }

        private void btnBaseRepair_Click(object sender, EventArgs e)
        {
            try
            {
                int num = this.ListChecks.Items.Count;
                for (int i = 0; i < num; i++)
                {
                    GKListItem item = (GKListItem)this.ListChecks.Items[i];
                    if (!item.Checked) continue;

                    TreeTools.CheckObj checkObj = item.Data as TreeTools.CheckObj;
                    TreeTools.RepairProblem(this.fBase, checkObj);
                }
            }
            finally
            {
                this.Base.RefreshLists(false);
                this.CheckBase();
            }
        }

        private void ListChecksDblClick(object sender, EventArgs e)
        {
            GKListItem item = this.ListChecks.SelectedItem();
            if (item == null) return;

            GEDCOMIndividualRecord iRec = ((TreeTools.CheckObj)item.Data).Rec as GEDCOMIndividualRecord;
            if (iRec == null) return;

            this.Base.SelectRecordByXRef(iRec.XRef);
            this.Close();
        }

        #endregion

        #region Places Management

        private void PreparePlacesList()
        {
            this.ListPlaces = GKUtils.CreateListView(this.Panel4);
            this.ListPlaces.DoubleClick += this.ListPlacesDblClick;
            this.ListPlaces.AddListColumn(LangMan.LS(LSID.LSID_Place), 400, false);
            this.ListPlaces.AddListColumn(LangMan.LS(LSID.LSID_LinksCount), 100, false);
        }

        private void CheckPlaces()
        {
            this.ListPlaces.BeginUpdate();
            try
            {
                TreeTools.PlacesSearch(this.fTree, this.fPlaces, fBase);

                this.ListPlaces.Items.Clear();

                int num4 = this.fPlaces.Count;
                for (int i = 0; i < num4; i++)
                {
                    PlaceObj placeObj = (PlaceObj)this.fPlaces.GetObject(i);

                    GKListItem item = this.ListPlaces.AddItem(this.fPlaces[i], placeObj);
                    item.AddSubItem(placeObj.Facts.Count);
                }
            }
            finally
            {
                this.ListPlaces.EndUpdate();
            }
        }

        private void btnIntoList_Click(object sender, EventArgs e)
        {
            this.ListPlacesDblClick(null, null);
        }

        private void ListPlacesDblClick(object sender, EventArgs e)
        {
            GKListItem item = this.ListPlaces.SelectedItem();
            if (item == null) return;
            
            PlaceObj pObj = item.Data as PlaceObj;
            if (pObj == null) return;
            
            if (pObj.Name.IndexOf("[*]") == 0)
            {
                GKUtils.ShowMessage(LangMan.LS(LSID.LSID_PlaceAlreadyInBook));
            }
            else
            {
                GEDCOMLocationRecord loc = this.Base.SelectRecord(GEDCOMRecordType.rtLocation, new object[] { pObj.Name }) as GEDCOMLocationRecord;

                if (loc != null)
                {
                    int num = pObj.Facts.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMCustomEvent evt = pObj.Facts[i];
                        evt.Detail.Place.StringValue = loc.LocationName;
                        evt.Detail.Place.Location.Value = loc;
                    }

                    this.CheckPlaces();

                    this.Base.RefreshLists(false);
                }
            }
        }

        #endregion

        #region Tree Splitting

        private void UpdateSplitLists()
        {
            this.ListSelected.BeginUpdate();
            this.ListSelected.Items.Clear();
            this.ListSkipped.BeginUpdate();
            this.ListSkipped.Items.Clear();
            try
            {
                int cnt = 0;

                int num = this.fTree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = this.fTree[i];
                    if (rec is GEDCOMIndividualRecord)
                    {
                        cnt++;
                        GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
                        string st = iRec.XRef + " / " + iRec.GetNameString(true, false);

                        if (this.fSplitList.IndexOf(iRec) < 0) {
                            this.ListSkipped.Items.Add(st);
                        } else {
                            this.ListSelected.Items.Add(st);
                        }
                    }
                }
                this.Text = this.fSplitList.Count.ToString() + @" / " + cnt.ToString();
            }
            finally
            {
                this.ListSelected.EndUpdate();
                this.ListSkipped.EndUpdate();
            }
        }

        private void Select(GEDCOMIndividualRecord startPerson, TreeTools.TreeWalkMode walkMode)
        {
            this.fSplitList.Clear();
            TreeTools.TreeWalk(startPerson, walkMode, this.fSplitList);
            this.UpdateSplitLists();
        }

        private void btnSelectFamily_Click(object sender, EventArgs e)
        {
            this.Select(this.Base.GetSelectedPerson(), TreeTools.TreeWalkMode.twmFamily);
        }

        private void btnSelectAncestors_Click(object sender, EventArgs e)
        {
            this.Select(this.Base.GetSelectedPerson(), TreeTools.TreeWalkMode.twmAncestors);
        }

        private void btnSelectDescendants_Click(object sender, EventArgs e)
        {
            this.Select(this.Base.GetSelectedPerson(), TreeTools.TreeWalkMode.twmDescendants);
        }

        private void btnSelectAll_Click(object sender, EventArgs e)
        {
            this.Select(this.Base.GetSelectedPerson(), TreeTools.TreeWalkMode.twmAll);
        }

        private void btnDelete_Click(object sender, EventArgs e)
        {
            int num = this.fSplitList.Count;
            for (int i = 0; i < num; i++)
            {
                object obj = this.fSplitList[i];

                if (obj is GEDCOMIndividualRecord)
                {
                    this.Base.RecordDelete(obj as GEDCOMIndividualRecord, false);
                }
            }

            GKUtils.ShowMessage(LangMan.LS(LSID.LSID_RecsDeleted));
            this.fSplitList.Clear();
            this.UpdateSplitLists();

            this.Base.RefreshLists(false);
        }

        private void btnSave_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetSaveFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT, "");
            if (!string.IsNullOrEmpty(fileName))
            {
                TreeTools.CheckRelations(fSplitList);

                string subm = this.fTree.Header.GetTagStringValue("SUBM");
                this.fTree.Header.Clear();
                this.fTree.Header.Source = "GEDKeeper";
                this.fTree.Header.ReceivingSystemName = "GEDKeeper";
                this.fTree.Header.CharacterSet = MainWin.Instance.Options.DefCharacterSet;
                this.fTree.Header.Language = "Russian";
                this.fTree.Header.GEDCOMVersion = "5.5";
                this.fTree.Header.GEDCOMForm = "LINEAGE-LINKED";
                this.fTree.Header.FileName = Path.GetFileName(fileName);
                this.fTree.Header.TransmissionDate.Date = DateTime.Now;

                if (subm != "") {
                    this.fTree.Header.SetTagStringValue("SUBM", subm);
                }

                using (StreamWriter fs = new StreamWriter(fileName, false, GEDCOMUtils.GetEncodingByCharacterSet(this.fTree.Header.CharacterSet)))
                {
                    this.fTree.SaveToStream(fs, this.fSplitList);
                    this.fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
                }
            }
        }

        #endregion

        #region Patriarchs Search

        private void PreparePatriarchsList()
        {
            this.ListPatriarchs = GKUtils.CreateListView(this.Panel3);
            this.ListPatriarchs.DoubleClick += this.ListPatriarchsDblClick;
            this.ListPatriarchs.AddListColumn(LangMan.LS(LSID.LSID_Patriarch), 400, false);
            this.ListPatriarchs.AddListColumn(LangMan.LS(LSID.LSID_Birth), 90, false);
            this.ListPatriarchs.AddListColumn(LangMan.LS(LSID.LSID_Descendants), 90, false);
            this.ListPatriarchs.AddListColumn(LangMan.LS(LSID.LSID_Generations), 90, false);
        }

        private void ListPatriarchsDblClick(object sender, EventArgs e)
        {
            GKListItem item = this.ListPatriarchs.SelectedItem();
            if (item == null) return;

            GEDCOMIndividualRecord iRec = item.Data as GEDCOMIndividualRecord;
            if (iRec == null) return;

            this.Base.SelectRecordByXRef(iRec.XRef);
            this.Close();
        }

        private void btnPatSearch_Click(object sender, EventArgs e)
        {
            this.ListPatriarchs.BeginUpdate();
            ExtList<PatriarchObj> lst = null;
            try
            {
                this.ListPatriarchs.Items.Clear();
                lst = this.fBase.Context.GetPatriarchsList(decimal.ToInt32(this.edMinGens.Value), !chkWithoutDates.Checked);

                int num = lst.Count;
                for (int i = 0; i < num; i++)
                {
                    PatriarchObj pObj = lst[i];
                    string pSign = ((pObj.IRec.Patriarch) ? "[*] " : "");

                    GKListItem item = this.ListPatriarchs.AddItem(pSign + pObj.IRec.GetNameString(true, false), pObj.IRec);
                    item.AddSubItem(pObj.BirthYear);
                    item.AddSubItem(pObj.DescendantsCount);
                    item.AddSubItem(pObj.DescGenerations);
                }
            }
            finally
            {
                if (lst != null) lst.Dispose();
                this.ListPatriarchs.EndUpdate();
            }
        }

        private void btnSetPatriarch_Click(object sender, EventArgs e)
        {
            try
            {
                GKListItem item = this.ListPatriarchs.SelectedItem();
                if (item == null) return;

                GEDCOMIndividualRecord iRec = item.Data as GEDCOMIndividualRecord;
                if (iRec != null)
                {
                    iRec.Patriarch = true;
                }

                this.Base.RefreshLists(false);
            }
            finally
            {
                this.btnPatSearch_Click(null, null);
            }
        }

        private void btnPatriarchsDiagram_Click(object sender, EventArgs e)
        {
            PatriarchsViewerWin wnd = new PatriarchsViewerWin(this.fBase, decimal.ToInt32(this.edMinGens.Value));
            wnd.Show();
        }

        #endregion

        #region Match files

        private string external_match_db;
        private enum TreeMatchType { tmtInternal, tmtExternal, tmtAnalysis }

        private void btnFileChoose_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetOpenFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName))
            {
                external_match_db = fileName;
                this.txtCompareFile.Text = Path.GetFileName(external_match_db);
            }
        }

        private void DuplicateFoundFunc(GEDCOMIndividualRecord indivA, GEDCOMIndividualRecord indivB)
        {
            this.ListCompare.AppendText("    * [" + indivA.GetNameString(true, false) + "]\r\n");
            this.ListCompare.AppendText("      [" + indivB.GetNameString(true, false) + "]\r\n\r\n");
            //this.ListCompare.AppendText("\r\n");
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

            this.ListCompare.Clear();

            switch (type) {
                case TreeMatchType.tmtInternal:
                    TreeTools.FindDuplicates(this.fTree, this.fTree, 90 /*min: 80-85*/, DuplicateFoundFunc, fBase);
                    break;

                case TreeMatchType.tmtExternal:
                    TreeTools.TreeCompare(this.fTree, external_match_db, this.ListCompare);
                    break;

                case TreeMatchType.tmtAnalysis:
                    {
                        List<TreeTools.ULIndividual> uln = TreeTools.GetUnlinkedNamesakes(this.fTree, fBase);

                        this.ListCompare.AppendText("  Поиск несвязанных однофамильцев:\r\n");
                        if (uln != null && uln.Count > 0)
                        {
                            int num = uln.Count;
                            foreach (TreeTools.ULIndividual indiv in uln)
                            {
                                this.ListCompare.AppendText("    - [" + indiv.Family + "] " + indiv.IRec.GetNameString(true, false) + "\r\n");
                            }
                        }
                        else
                        {
                            this.ListCompare.AppendText("    - not found.");
                        }
                        break;
                    }
            }
        }

        private void rbtnMatch_CheckedChanged(object sender, EventArgs e)
        {
            TreeMatchType type = GetTreeMatchType();

            this.lblFile.Enabled = (type == TreeMatchType.tmtExternal);
            this.txtCompareFile.Enabled = (type == TreeMatchType.tmtExternal);
            this.btnFileChoose.Enabled = (type == TreeMatchType.tmtExternal);
        }

        #endregion
    }
}
