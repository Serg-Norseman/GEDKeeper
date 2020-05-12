﻿/*
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
using System.IO;
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKNavigatorPlugin
{
    public enum DataCategory
    {
        Root,
        RecentActivity,
        JumpHistory,
        PotencialProblems,
        Filters,
        Bookmarks,
        Records
    }

    public partial class NavigatorWidget : Form, ILocalization
    {
        private readonly Plugin fPlugin;

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

        public NavigatorWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;

            InitControls();
            SetLang();
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

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_Navigator);

            // FIXME: translations
            tnRecAct.Text = "Recent Activity";
            tnJumpHist.Text = "Jump history";
            tnProblems.Text = "Potencial problems";
            tnFilters.Text = "Filters";
            tnBookmarks.Text = "Bookmarks";
        }

        #endregion

        private void NavigatorWidget_Load(object sender, EventArgs e)
        {
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
            if (fBase != baseWin && fBase != null)
            {
            }

            fBase = baseWin;

            if (fBase != null) {
                fDatabaseName = Path.GetFileName(fBase.Context.FileName);
            } else {
                fDatabaseName = "";
            }

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
                    tnRecsIndividual.Text = FmtTitle("Individuals", stats[(int)GDMRecordType.rtIndividual]);
                    tnRecsFamily.Text = FmtTitle("Families", stats[(int)GDMRecordType.rtFamily]);
                    tnRecsNote.Text = FmtTitle("Notes", stats[(int)GDMRecordType.rtNote]);
                    tnRecsMultimedia.Text = FmtTitle("Multimedia", stats[(int)GDMRecordType.rtMultimedia]);
                    tnRecsSource.Text = FmtTitle("Sources", stats[(int)GDMRecordType.rtSource]);
                    tnRecsRepository.Text = FmtTitle("Repositories", stats[(int)GDMRecordType.rtRepository]);
                    tnRecsGroup.Text = FmtTitle("Groups", stats[(int)GDMRecordType.rtGroup]);
                    tnRecsResearch.Text = FmtTitle("Researches", stats[(int)GDMRecordType.rtResearch]);
                    tnRecsTask.Text = FmtTitle("Tasks", stats[(int)GDMRecordType.rtTask]);
                    tnRecsCommunication.Text = FmtTitle("Communications", stats[(int)GDMRecordType.rtCommunication]);
                    tnRecsLocation.Text = FmtTitle("Locations", stats[(int)GDMRecordType.rtLocation]);

                    treeView1.ExpandAll();
                } finally {
                    treeView1.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKNavigatorPlugin.UpdateControls(): " + ex.Message);
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
            switch (category)
            {
                case DataCategory.RecentActivity:
                    lvData.Clear();
                    break;

                case DataCategory.JumpHistory:
                    lvData.Clear();
                    break;

                case DataCategory.PotencialProblems:
                    lvData.Clear();
                    break;

                case DataCategory.Filters:
                    lvData.Clear();
                    break;

                case DataCategory.Bookmarks:
                    lvData.Clear();
                    break;

                case DataCategory.Records:
                    fBase.ShowRecordsTab(recordType);
                    ShowRecordsData(recordType);
                    break;
            }
        }

        private void ShowRecordsData(GDMRecordType recordType)
        {
            lvData.BeginUpdate();
            try
            {
                lvData.Clear();
                lvData.AddColumn("Action", 20, true);
                lvData.AddColumn("XRef", 20, true);
                lvData.AddColumn("Name", 20, true);
                lvData.AddColumn("Time", 20, true);

                BaseData baseData = fPlugin.Data[fBase.Context.FileName];
                if (baseData == null) return;

                foreach (var recordInfo in baseData.ChangedRecords)
                {
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

                    lvData.AddItem(null, new object[] { act,
                                       recordInfo.XRef,
                                       recordInfo.Name,
                                       recordInfo.Time.ToString() });
                }
            }
            finally
            {
                lvData.EndUpdate();
            }
        }
    }
}
