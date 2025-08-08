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

#pragma warning disable CS0618

using System;
using System.IO;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Plugins;
using GKUI.Components;

namespace GKNavigatorPlugin
{
    public partial class NavigatorWidget : Form, IWidgetForm
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Panel placeholder;
        private TreeView treeView1;
        private GKListView lvData;
        private WebLinksView webLinksView;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly Plugin fPlugin;
        private readonly ILangMan fLangMan;

        private IBaseWindow fBase;
        private string fDatabaseName;

        private GKTreeNode tnHiddenRoot;
        private GKTreeNode tnRoot;
        private GKTreeNode tnRecAct;
        private GKTreeNode tnJumpHist;
        private GKTreeNode tnProblems;
        private GKTreeNode tnBookmarks;
        private GKTreeNode tnRecords;
        private GKTreeNode tnLanguages;
        private GKTreeNode tnAssociations;
        private GKTreeNode tnWebLinks;

        public NavigatorWidget(Plugin plugin)
        {
            XamlReader.Load(this);

            fPlugin = plugin;
            fLangMan = fPlugin.LangMan;

            InitControls();
            SetLocale();
        }

        private void InitControls()
        {
            treeView1.DataStore = null;

            tnHiddenRoot = new GKTreeNode("root", null);
            treeView1.DataStore = tnHiddenRoot;

            tnRoot = CreateNode(tnHiddenRoot, "root", null);
            tnRecAct = CreateNode(tnRoot, "Recent Activity", DataCategory.RecentActivity);
            tnJumpHist = CreateNode(tnRecAct, "Jump history", DataCategory.JumpHistory);
            tnProblems = CreateNode(tnRecAct, "Potencial problems", DataCategory.PotencialProblems);
            tnBookmarks = CreateNode(tnRoot, "Bookmarks", DataCategory.Bookmarks);
            tnLanguages = CreateNode(tnRoot, "Languages", DataCategory.Languages);
            tnAssociations = CreateNode(tnRoot, "Associations", DataCategory.Associations);
            tnWebLinks = CreateNode(tnRoot, "WebLinks", DataCategory.WebLinks);
            tnRecords = CreateNode(tnRecAct, "Records", DataCategory.Records);

            lvData = new GKListView();
            lvData.Size = new Size(391, 498);
            lvData.MouseDown += lvData_SelectedIndexChanged;

            webLinksView = new WebLinksView();
            webLinksView.Size = new Size(391, 498);
        }

        public void SetLocale()
        {
            Title = fLangMan.LS(PLS.Navigator);
            tnRecAct.Text = fLangMan.LS(PLS.RecentActivity);
            tnJumpHist.Text = fLangMan.LS(PLS.JumpHistory);
            tnProblems.Text = fLangMan.LS(PLS.PotencialProblems);
            tnBookmarks.Text = fLangMan.LS(PLS.Bookmarks);
            tnLanguages.Text = fLangMan.LS(PLS.Languages);
            tnAssociations.Text = LangMan.LS(LSID.Associations);
            tnRecords.Text = fLangMan.LS(PLS.Records);
            tnWebLinks.Text = fLangMan.LS(PLS.WebLinks);
        }

        private void Form_Shown(object sender, EventArgs e)
        {
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VBottom);
            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            NavigatorData.SelectLanguage(fBase, GDMLanguageID.Unknown);
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
            fPlugin.CloseForm();
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
                    //treeView1.BeginUpdate();
                    tnRoot.Text = dbName;
                    tnRecords.Text = FmtTitle(fLangMan.LS(PLS.Records), changedRecs);

                    tnHiddenRoot.Expanded = true;
                    tnRoot.Expanded = true;
                    tnRecAct.Expanded = true;
                    tnRecords.Expanded = true;
                } finally {
                    //treeView1.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKNavigatorPlugin.UpdateControls()", ex);
            }
        }

        private GKTreeNode CreateNode(GKTreeNode parent, string title, object tag)
        {
            var node = new GKTreeNode(title, tag);
            if (parent == null) {
                tnRoot.Children.Add(node);
            } else {
                ((GKTreeNode)parent).Children.Add(node);
            }
            return node;
        }

        private void TreeView1AfterSelect(object sender, EventArgs e)
        {
            if (treeView1.SelectedItem == null) return;
            object tag = ((GKTreeNode)treeView1.SelectedItem).Tag;
            if (tag == null) return;

            if (tag is DataCategory dataCat && dataCat == DataCategory.WebLinks) {
                placeholder.Content = webLinksView;
            } else {
                placeholder.Content = lvData;
            }

            fPlugin.Data.ShowItem(fBase, tag, lvData);
        }

        private void lvData_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            object tag = ((GKTreeNode)treeView1.SelectedItem).Tag;
            var itemData = lvData.GetSelectedData();
            NavigatorData.SelectItem(fBase, tag, itemData);
        }
    }
}
