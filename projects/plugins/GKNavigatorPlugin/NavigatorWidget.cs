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

using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Plugins;
using GKUI.Components;
using GKUI.Themes;

namespace GKNavigatorPlugin
{
    public partial class NavigatorWidget : Form, IWidgetForm, IThemedView
    {
        #region Design components

        private GKListView lvData;
        private WebLinksView webLinksView;

        #endregion

        private readonly Plugin fPlugin;
        private readonly ILangMan fLangMan;

        private IBaseWindow fBase;
        private string fDatabaseName;

        private TreeNode tnRoot;
        private TreeNode tnRecAct;
        private TreeNode tnJumpHist;
        private TreeNode tnProblems;
        private TreeNode tnBookmarks;
        private TreeNode tnRecords;
        private TreeNode tnLanguages;
        private TreeNode tnWebLinks;

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
            tnBookmarks = CreateNode(tnRoot, "Bookmarks", DataCategory.Bookmarks);
            tnLanguages = CreateNode(tnRoot, "Languages", DataCategory.Languages);
            tnWebLinks = CreateNode(tnRoot, "WebLinks", DataCategory.WebLinks);
            tnRecords = CreateNode(tnRecAct, "Records", DataCategory.Records);

            lvData = new GKListView();
            lvData.Dock = DockStyle.Fill;
            lvData.Location = new Point(0, 0);
            lvData.Name = "lvData";
            lvData.Size = new Size(391, 498);
            lvData.SelectedIndexChanged += lvData_SelectedIndexChanged;

            webLinksView = new WebLinksView();
            webLinksView.Size = new Size(391, 498);
        }

        public void SetLocale()
        {
            Text = fLangMan.LS(PLS.Navigator);
            tnRecAct.Text = fLangMan.LS(PLS.RecentActivity);
            tnJumpHist.Text = fLangMan.LS(PLS.JumpHistory);
            tnProblems.Text = fLangMan.LS(PLS.PotencialProblems);
            tnBookmarks.Text = fLangMan.LS(PLS.Bookmarks);
            tnLanguages.Text = fLangMan.LS(PLS.Languages);
            tnRecords.Text = fLangMan.LS(PLS.Records);
            tnWebLinks.Text = fLangMan.LS(PLS.WebLinks);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            if (!DesignMode && AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VBottom);

            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            NavigatorData.SelectLanguage(fBase, GDMLanguageID.Unknown);
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
            fPlugin.CloseForm();
        }

        public void ApplyTheme()
        {
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
                int changedRecs = 0;
                if (fBase == null) {
                    dbName = "";
                } else {
                    dbName = fDatabaseName;

                    BaseData baseData = fPlugin.Data[fBase.Context.FileName];
                    if (baseData != null) changedRecs = baseData.ChangedRecords.Count;
                }

                try {
                    treeView1.BeginUpdate();

                    tnRoot.Text = dbName;
                    tnRecords.Text = FmtTitle(fLangMan.LS(PLS.Records), changedRecs);

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

            if (tag is DataCategory dataCat && dataCat == DataCategory.WebLinks) {
                placeholder.Controls.Clear();
                placeholder.Controls.Add(webLinksView);
            } else {
                placeholder.Controls.Clear();
                placeholder.Controls.Add(lvData);
            }

            fPlugin.Data.ShowItem(fBase, tag, lvData);
        }

        private void lvData_SelectedIndexChanged(object sender, EventArgs e)
        {
            object tag = treeView1.SelectedNode.Tag;
            var itemData = lvData.GetSelectedData();
            NavigatorData.SelectItem(fBase, tag, itemData);
        }
    }
}
