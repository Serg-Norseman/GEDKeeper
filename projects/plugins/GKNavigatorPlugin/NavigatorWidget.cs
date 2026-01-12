/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
