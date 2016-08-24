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
using System.IO;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKNavigatorPlugin
{
    public partial class NavigatorWidget : Form, ILocalization
    {
        private readonly Plugin fPlugin;

        private IBaseWindow fBase;
        private string fDatabaseName;

        TreeNode root;
        TreeNode recAct;
        TreeNode jumpHist;
        TreeNode problems;
        TreeNode filters;
        TreeNode bookmarks;
        TreeNode records;
        TreeNode recsIndividual;
        TreeNode recsFamily;
        TreeNode recsNote;
        TreeNode recsMultimedia;
        TreeNode recsSource;
        TreeNode recsRepository;
        TreeNode recsGroup;
        TreeNode recsResearch;
        TreeNode recsTask;
        TreeNode recsCommunication;
        TreeNode recsLocation;

        public NavigatorWidget(Plugin plugin)
        {
            InitializeComponent();

            this.fPlugin = plugin;

            this.InitControls();

            this.SetLang();
        }

        private void NavigatorWidget_Load(object sender, EventArgs e)
        {
            this.fPlugin.Host.WidgetShow(this.fPlugin);
            this.BaseChanged(this.fPlugin.Host.GetCurrentFile());
        }

        private void NavigatorWidget_Closed(object sender, EventArgs e)
        {
            this.BaseChanged(null);
            this.fPlugin.Host.WidgetClose(this.fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (this.fBase != baseWin && this.fBase != null)
            {
            }

            this.fBase = baseWin;

            if (this.fBase != null) {
                this.fDatabaseName = Path.GetFileName(this.fBase.Tree.FileName);
            } else {
                this.fDatabaseName = "";
            }

            this.UpdateControls();
        }

        private static string FmtTitle(string title, int count)
        {
            return string.Format("{0} ({1})", title, count);
        }

        private void UpdateControls()
        {
            if (this.fBase == null) return;

            try {
                try {
                    this.treeView1.BeginUpdate();

                    root.Text = this.fDatabaseName;

                    /*TreeNode recAct = root.Nodes.Add("Recent Activity");
            TreeNode jumpHist = recAct.Nodes.Add("Jump history");
            TreeNode problems = recAct.Nodes.Add("Potencial problems");
            TreeNode filters = recAct.Nodes.Add("Filters");*/

                    //TreeNode bookmarks = root.Nodes.Add("Bookmarks");
                    //TreeNode records = root.Nodes.Add("Records");

                    int[] stats = this.fBase.Tree.GetRecordStats();
                    recsIndividual.Text = FmtTitle("Individuals", stats[(int)GEDCOMRecordType.rtIndividual]);
                    recsFamily.Text = FmtTitle("Families", stats[(int)GEDCOMRecordType.rtFamily]);
                    recsNote.Text = FmtTitle("Notes", stats[(int)GEDCOMRecordType.rtNote]);
                    recsMultimedia.Text = FmtTitle("Multimedia", stats[(int)GEDCOMRecordType.rtMultimedia]);
                    recsSource.Text = FmtTitle("Sources", stats[(int)GEDCOMRecordType.rtSource]);
                    recsRepository.Text = FmtTitle("Repositories", stats[(int)GEDCOMRecordType.rtRepository]);
                    recsGroup.Text = FmtTitle("Groups", stats[(int)GEDCOMRecordType.rtGroup]);
                    recsResearch.Text = FmtTitle("Researches", stats[(int)GEDCOMRecordType.rtResearch]);
                    recsTask.Text = FmtTitle("Tasks", stats[(int)GEDCOMRecordType.rtTask]);
                    recsCommunication.Text = FmtTitle("Communications", stats[(int)GEDCOMRecordType.rtCommunication]);
                    recsLocation.Text = FmtTitle("Locations", stats[(int)GEDCOMRecordType.rtLocation]);

                    this.treeView1.ExpandAll();
                } finally {
                    this.treeView1.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                this.fPlugin.Host.LogWrite("GKNavigatorPlugin.UpdateControls(): " + ex.Message);
            }
        }

        private void InitControls()
        {
            this.treeView1.Nodes.Clear();

            root = this.treeView1.Nodes.Add("root");

            recAct = root.Nodes.Add("Recent Activity");
            jumpHist = recAct.Nodes.Add("Jump history");
            problems = recAct.Nodes.Add("Potencial problems");
            filters = recAct.Nodes.Add("Filters");

            bookmarks = root.Nodes.Add("Bookmarks");
            records = root.Nodes.Add("Records");

            recsIndividual = CreateRecordTypeNode("Individuals", GEDCOMRecordType.rtIndividual);
            recsFamily = CreateRecordTypeNode("Families", GEDCOMRecordType.rtFamily);
            recsNote = CreateRecordTypeNode("Notes", GEDCOMRecordType.rtNote);
            recsMultimedia = CreateRecordTypeNode("Multimedia", GEDCOMRecordType.rtMultimedia);
            recsSource = CreateRecordTypeNode("Sources", GEDCOMRecordType.rtSource);
            recsRepository = CreateRecordTypeNode("Repositories", GEDCOMRecordType.rtRepository);
            recsGroup = CreateRecordTypeNode("Groups", GEDCOMRecordType.rtGroup);
            recsResearch = CreateRecordTypeNode("Researches", GEDCOMRecordType.rtResearch);
            recsTask = CreateRecordTypeNode("Tasks", GEDCOMRecordType.rtTask);
            recsCommunication = CreateRecordTypeNode("Communications", GEDCOMRecordType.rtCommunication);
            recsLocation = CreateRecordTypeNode("Locations", GEDCOMRecordType.rtLocation);
        }

        private TreeNode CreateRecordTypeNode(string title, GEDCOMRecordType recType)
        {
            TreeNode result = records.Nodes.Add(title);
            result.Tag = recType;
            return result;
        }

        private void CollectData()
        {
        }

        #region ILocalization support

        public void SetLang()
        {
            this.Text = this.fPlugin.LangMan.LS(PLS.LSID_Navigator);
        }

        #endregion

        private void TreeView1AfterSelect(object sender, TreeViewEventArgs e)
        {
            object tag = e.Node.Tag;
            if (tag != null && tag.GetType() == typeof(GEDCOMRecordType)) {
                this.fBase.ShowRecordsTab((GEDCOMRecordType)tag);
            }
        }
    }
}
