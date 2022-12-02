/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.IO;
using System.Windows.Forms;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKNavigatorPlugin
{
    public partial class NavigatorWidget : Form, IWidgetForm
    {
        private readonly Plugin fPlugin;
        private readonly ILangMan fLangMan;

        private IBaseWindow fBase;
        private string fDatabaseName;

        private TreeNode tnRoot;
        private TreeNode tnRecAct;
        private TreeNode tnJumpHist;
        private TreeNode tnProblems;
        private TreeNode tnFilters;
        private TreeNode tnBookmarks;
        private TreeNode tnRecords;
        private TreeNode tnRecsIndividual;
        private TreeNode tnRecsFamily;
        private TreeNode tnRecsNote;
        private TreeNode tnRecsMultimedia;
        private TreeNode tnRecsSource;
        private TreeNode tnRecsRepository;
        private TreeNode tnRecsGroup;
        private TreeNode tnRecsResearch;
        private TreeNode tnRecsTask;
        private TreeNode tnRecsCommunication;
        private TreeNode tnRecsLocation;
        private TreeNode tnLanguages;

        public NavigatorWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            fLangMan = fPlugin.LangMan;

            InitControls();
            SetLocale();
        }

        private void InitControls()
        {
            treeView1.Nodes.Clear();

            tnRoot = treeView1.Nodes.Add("root");

            tnRecAct = CreateNode(tnRoot, "Recent Activity", DataCategory.RecentActivity);
            tnJumpHist = CreateNode(tnRecAct, "Jump history", DataCategory.JumpHistory);
            tnProblems = CreateNode(tnRecAct, "Potencial problems", DataCategory.PotencialProblems);
            tnFilters = CreateNode(tnRecAct, "Filters", DataCategory.Filters);

            tnBookmarks = CreateNode(tnRoot, "Bookmarks", DataCategory.Bookmarks);
            tnLanguages = CreateNode(tnRoot, "Languages", DataCategory.Languages);

            tnRecords = tnRoot.Nodes.Add("Records");
            tnRecsIndividual = CreateNode(tnRecords, "Individuals", GDMRecordType.rtIndividual);
            tnRecsFamily = CreateNode(tnRecords, "Families", GDMRecordType.rtFamily);
            tnRecsNote = CreateNode(tnRecords, "Notes", GDMRecordType.rtNote);
            tnRecsMultimedia = CreateNode(tnRecords, "Multimedia", GDMRecordType.rtMultimedia);
            tnRecsSource = CreateNode(tnRecords, "Sources", GDMRecordType.rtSource);
            tnRecsRepository = CreateNode(tnRecords, "Repositories", GDMRecordType.rtRepository);
            tnRecsGroup = CreateNode(tnRecords, "Groups", GDMRecordType.rtGroup);
            tnRecsResearch = CreateNode(tnRecords, "Researches", GDMRecordType.rtResearch);
            tnRecsTask = CreateNode(tnRecords, "Tasks", GDMRecordType.rtTask);
            tnRecsCommunication = CreateNode(tnRecords, "Communications", GDMRecordType.rtCommunication);
            tnRecsLocation = CreateNode(tnRecords, "Locations", GDMRecordType.rtLocation);
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Text = fLangMan.LS(PLS.LSID_Navigator);
            tnRecAct.Text = fLangMan.LS(PLS.LSID_RecentActivity);
            tnJumpHist.Text = fLangMan.LS(PLS.LSID_JumpHistory);
            tnProblems.Text = fLangMan.LS(PLS.LSID_PotencialProblems);
            tnFilters.Text = fLangMan.LS(PLS.LSID_Filters);
            tnBookmarks.Text = fLangMan.LS(PLS.LSID_Bookmarks);
            tnLanguages.Text = fLangMan.LS(PLS.LSID_Languages);
            tnRecords.Text = fLangMan.LS(PLS.LSID_Records);
        }

        #endregion

        private void NavigatorWidget_Load(object sender, EventArgs e)
        {
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VBottom);

            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void NavigatorWidget_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin && fBase != null) {
            }

            fBase = baseWin;

            fDatabaseName = (fBase == null) ? string.Empty : Path.GetFileName(fBase.Context.FileName);

            UpdateControls();
        }

        public void BaseClosed(IBaseWindow baseWin)
        {
            fPlugin.Data.CloseBase(baseWin.Context.FileName);
            fDatabaseName = "";
            fBase = null;
            UpdateControls(true);
        }

        private static string FmtTitle(string title, int count)
        {
            return string.Format("{0} ({1})", title, count);
        }

        private void UpdateControls(bool afterClose = false)
        {
            try {
                string dbName;
                int[] stats;

                if (fBase == null) {
                    dbName = "";
                    stats = new int[((int)GDMRecordType.rtLast)];
                } else {
                    dbName = fDatabaseName;
                    stats = fBase.Context.Tree.GetRecordStats();
                }

                try {
                    treeView1.BeginUpdate();

                    tnRoot.Text = dbName;
                    tnRecsIndividual.Text = FmtTitle(fLangMan.LS(PLS.LSID_Individuals), stats[(int)GDMRecordType.rtIndividual]);
                    tnRecsFamily.Text = FmtTitle(fLangMan.LS(PLS.LSID_Families), stats[(int)GDMRecordType.rtFamily]);
                    tnRecsNote.Text = FmtTitle(fLangMan.LS(PLS.LSID_Notes), stats[(int)GDMRecordType.rtNote]);
                    tnRecsMultimedia.Text = FmtTitle(fLangMan.LS(PLS.LSID_Multimedia), stats[(int)GDMRecordType.rtMultimedia]);
                    tnRecsSource.Text = FmtTitle(fLangMan.LS(PLS.LSID_Sources), stats[(int)GDMRecordType.rtSource]);
                    tnRecsRepository.Text = FmtTitle(fLangMan.LS(PLS.LSID_Repositories), stats[(int)GDMRecordType.rtRepository]);
                    tnRecsGroup.Text = FmtTitle(fLangMan.LS(PLS.LSID_Groups), stats[(int)GDMRecordType.rtGroup]);
                    tnRecsResearch.Text = FmtTitle(fLangMan.LS(PLS.LSID_Researches), stats[(int)GDMRecordType.rtResearch]);
                    tnRecsTask.Text = FmtTitle(fLangMan.LS(PLS.LSID_Tasks), stats[(int)GDMRecordType.rtTask]);
                    tnRecsCommunication.Text = FmtTitle(fLangMan.LS(PLS.LSID_Communications), stats[(int)GDMRecordType.rtCommunication]);
                    tnRecsLocation.Text = FmtTitle(fLangMan.LS(PLS.LSID_Locations), stats[(int)GDMRecordType.rtLocation]);

                    treeView1.ExpandAll();
                } finally {
                    treeView1.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKNavigatorPlugin.UpdateControls()", ex);
            }
        }

        private TreeNode CreateNode(TreeNode parent, string title, object tag)
        {
            TreeNode result = parent.Nodes.Add(title);
            result.Tag = tag;
            return result;
        }

        private void TreeView1AfterSelect(object sender, TreeViewEventArgs e)
        {
            if (e.Node == null) return;

            object tag = e.Node.Tag;
            if (tag == null) return;

            if (tag is GDMRecordType) {
                ShowData(DataCategory.Records, (GDMRecordType)tag);
            } else if (tag is DataCategory) {
                ShowData((DataCategory)tag, GDMRecordType.rtNone);
            }
        }

        private void ShowData(DataCategory category, GDMRecordType recordType)
        {
            switch (category) {
                case DataCategory.RecentActivity:
                    lvData.Clear();
                    break;

                case DataCategory.JumpHistory:
                    ShowJumpHistory();
                    break;

                case DataCategory.PotencialProblems:
                    lvData.Clear();
                    break;

                case DataCategory.Filters:
                    ShowFilters();
                    break;

                case DataCategory.Bookmarks:
                    ShowBookmarks();
                    break;

                case DataCategory.Records:
                    ShowRecordsData(recordType);
                    break;

                case DataCategory.Languages:
                    ShowLanguages();
                    break;
            }
        }

        private void lvData_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            var itemData = lvData.GetSelectedData();
            if (itemData == null) return;

            object tag = treeView1.SelectedNode.Tag;
            if (tag == null) return;

            if (tag is GDMRecordType) {
                SelectRecordInfo((RecordInfo)itemData);
            } else if (tag is DataCategory) {
                var dataCat = (DataCategory)tag;

                switch (dataCat) {
                    case DataCategory.JumpHistory:
                        SelectRecord((GDMRecord)itemData);
                        break;

                    case DataCategory.Filters:
                        SelectFilter((FilterInfo)itemData);
                        break;

                    case DataCategory.Bookmarks:
                        SelectRecord((GDMRecord)itemData);
                        break;

                    case DataCategory.Languages:
                        SelectLanguage((GDMLanguageID)itemData);
                        break;
                }
            }
        }

        private void SelectRecord(GDMRecord iRec)
        {
            fBase.SelectByRec(iRec);
        }

        #region Records Data

        private void ShowRecordsData(GDMRecordType recordType)
        {
            fBase.ShowRecordsTab(recordType);

            lvData.BeginUpdate();
            try {
                lvData.Clear();
                lvData.AddColumn(fLangMan.LS(PLS.LSID_Action), 20, true);
                lvData.AddColumn("XRef", 20, true);
                lvData.AddColumn(fLangMan.LS(PLS.LSID_Name), 20, true);
                lvData.AddColumn(fLangMan.LS(PLS.LSID_Time), 20, true);

                BaseData baseData = fPlugin.Data[fBase.Context.FileName];
                if (baseData == null) return;

                foreach (var recordInfo in baseData.ChangedRecords) {
                    if (recordInfo.Type != recordType) continue;

                    string act = "";
                    switch (recordInfo.Action) {
                        case RecordAction.raAdd:
                            act = "+";
                            break;
                        case RecordAction.raEdit:
                            act = "*";
                            break;
                        case RecordAction.raDelete:
                            act = "-";
                            break;
                    }

                    lvData.AddItem(recordInfo, new object[] { act, recordInfo.XRef, recordInfo.Name, recordInfo.Time.ToString() });
                }

                lvData.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            } finally {
                lvData.EndUpdate();
            }
        }

        private void SelectRecordInfo(RecordInfo recInfo)
        {
            if (recInfo.Action != RecordAction.raDelete)
                SelectRecord(recInfo.Record);
        }

        #endregion

        #region JumpHistory

        private void ShowJumpHistory()
        {
            var tree = fBase.Context.Tree;
            var navArray = fBase.Navman.FullArray;

            lvData.BeginUpdate();
            try {
                lvData.Clear();
                lvData.AddColumn(fLangMan.LS(PLS.LSID_Record), 400);

                foreach (var rec in navArray) {
                    lvData.AddItem(rec, new object[] { GKUtils.GetRecordName(tree, rec, true) });
                }

                lvData.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            } finally {
                lvData.EndUpdate();
            }
        }

        #endregion

        #region Filters

        private void ShowFilters()
        {
            fBase.ShowRecordsTab(GDMRecordType.rtIndividual);

            lvData.BeginUpdate();
            try {
                lvData.Clear();
                lvData.AddColumn(fLangMan.LS(PLS.LSID_Filter), 400);

                BaseData baseData = fPlugin.Data[fBase.Context.FileName];
                if (baseData == null) return;

                foreach (var filterInfo in baseData.ChangedFilters) {
                    lvData.AddItem(filterInfo, new object[] { filterInfo.FilterView });
                }

                lvData.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            } finally {
                lvData.EndUpdate();
            }
        }

        private void SelectFilter(FilterInfo filterInfo)
        {
            try {
                fBase.ShowRecordsTab(filterInfo.RecType);
                filterInfo.ListSource.Filter.Deserialize(filterInfo.FilterContent);
                fBase.ApplyFilter(filterInfo.RecType);
            } catch (Exception ex) {
                Console.WriteLine(ex.Message);
            }
        }

        #endregion

        #region Bookmarks

        private void ShowBookmarks()
        {
            fBase.ShowRecordsTab(GDMRecordType.rtIndividual);

            var bookmarks = fPlugin.Data.SearchBookmarks(fBase.Context);

            lvData.BeginUpdate();
            try {
                lvData.Clear();
                lvData.AddColumn(fLangMan.LS(PLS.LSID_Person), 400);

                foreach (var iRec in bookmarks) {
                    lvData.AddItem(iRec, new object[] { GKUtils.GetNameString(iRec, true, false) });
                }

                lvData.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            } finally {
                lvData.EndUpdate();
            }
        }

        #endregion

        #region Languages

        private void ShowLanguages()
        {
            lvData.BeginUpdate();
            try {
                lvData.Clear();
                lvData.AddColumn(fLangMan.LS(PLS.LSID_Language), 200);

                var baseContext = fBase.Context;

                foreach (var lang in baseContext.LangsList) {
                    lvData.AddItem(lang, new object[] { GEDCOMUtils.GetLanguageStr(lang) });
                }

                lvData.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            } finally {
                lvData.EndUpdate();
            }
        }

        private void SelectLanguage(GDMLanguageID lang)
        {
            fBase.Context.DefaultLanguage = lang;
            fBase.ShowRecordsTab(GDMRecordType.rtIndividual);
            fBase.RefreshRecordsView(GDMRecordType.rtIndividual);
        }

        #endregion
    }
}
