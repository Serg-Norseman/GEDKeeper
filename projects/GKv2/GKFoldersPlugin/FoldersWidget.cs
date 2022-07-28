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
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKFoldersPlugin
{
    public partial class FoldersWidget : Form, ILocalizable
    {
        private readonly Plugin fPlugin;

        private IBaseWindow fBase;
        private StringList fFolders;
        private string fFilterFolder;

        public FoldersWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            fFolders = new StringList();
            fFolders.DuplicateSolve = DuplicateSolve.Ignore;
            fFolders.Sorted = true;
            fFilterFolder = string.Empty;

            SetLocale();
        }

        private void Form_Resize(object sender, EventArgs e)
        {
            Location = new Point(Screen.PrimaryScreen.WorkingArea.Width - Width - 10, 50);
        }

        private void FoldersWidget_Load(object sender, EventArgs e)
        {
            var loc = AppHost.Instance.WidgetLocate(UIHelper.Rt2Rt(this.Bounds), WidgetHorizontalLocation.Right, WidgetVerticalLocation.Top);
            this.Location = new Point(loc.X, loc.Y);

            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void FoldersWidget_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin) {
                // restore filter's default state
                if (fBase != null) {
                    fBase.SetExternalFilter(null);
                    fBase.ApplyFilter();
                }

                fBase = baseWin;
                fFolders.Clear();
                fFilterFolder = string.Empty;

                if (fBase != null) {
                    CollectData();
                    fBase.SetExternalFilter(FilterHandler);
                    fBase.ApplyFilter();
                }

                UpdateControls();
            }
        }

        private void CollectData()
        {
            int num = fBase.Context.Tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fBase.Context.Tree[i];
                if (FoldersHelper.HasFolderSupport(rec.RecordType)) {
                    string folder = FoldersHelper.GetFolder(rec);
                    if (!string.IsNullOrEmpty(folder)) {
                        fFolders.Add(folder);
                    }
                }
            }
        }

        private void UpdateControls()
        {
            cmbFilterFolders.Items.Clear();
            cmbFilterFolders.Items.Add("");
            cmbFilterFolders.Items.AddRange(fFolders.ToArray());

            cmbSelectFolder.Items.Clear();
            cmbSelectFolder.Items.Add("");
            cmbSelectFolder.Items.AddRange(fFolders.ToArray());

            UpdateButtons(fBase);
        }

        private bool FilterHandler(GDMRecord record)
        {
            bool result = true;
            try {
                if (!string.IsNullOrEmpty(fFilterFolder) && FoldersHelper.HasFolderSupport(record.RecordType)) {
                    result = fFilterFolder.Equals(FoldersHelper.GetFolder(record));
                }
            } catch (Exception ex) {
                Logger.WriteError("FoldersWidget.FilterHandler()", ex);
            }
            return result;
        }

        private void btnSetFilter_Click(object sender, EventArgs e)
        {
            fFilterFolder = cmbFilterFolders.Text;
            if (fBase != null) {
                fBase.ApplyFilter();
            }
        }

        private void btnSetCurrent_Click(object sender, EventArgs e)
        {
            string folder = cmbSelectFolder.Text;
            if (fBase != null) {
                var record = fBase.GetSelectedRecordEx();

                if (SetRecordFolder(record, folder)) {
                    AppHost.StdDialogs.ShowMessage(string.Format("Processed {0} record(s)", 1));
                }

                ModifyBase();
                cmbSelectFolder.Text = "";
            }
        }

        private void btnSetSelected_Click(object sender, EventArgs e)
        {
            string folder = cmbSelectFolder.Text;
            if (fBase != null) {
                var recType = fBase.GetSelectedRecordType();
                var records = fBase.GetContentList(recType);

                int changed = 0;
                int num = records.Count;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = records[i];
                    if (SetRecordFolder(rec, folder)) {
                        changed += 1;
                    }
                }
                AppHost.StdDialogs.ShowMessage(string.Format("Processed {0} record(s)", changed));

                ModifyBase();
                cmbSelectFolder.Text = "";
            }
        }

        private void btnSetAll_Click(object sender, EventArgs e)
        {
            string folder = cmbSelectFolder.Text;
            if (fBase != null) {
                var tree = fBase.Context.Tree;

                int changed = 0;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = tree[i];
                    if (SetRecordFolder(rec, folder)) {
                        changed += 1;
                    }
                }
                AppHost.StdDialogs.ShowMessage(string.Format("Processed {0} record(s)", changed));

                ModifyBase();
                cmbSelectFolder.Text = "";
            }
        }

        private void ModifyBase()
        {
            fBase.Context.Modified = true;
            CollectData();
            UpdateControls();
        }

        private bool SetRecordFolder(GDMRecord record, string folder)
        {
            if (record != null && FoldersHelper.HasFolderSupport(record.RecordType)) {
                FoldersHelper.SetFolder(record, folder);
                return true;
            }
            return false;
        }

        public void SelectedIndexChanged(IBaseWindow baseWin)
        {
            UpdateButtons(baseWin);
        }

        public void TabChanged(IBaseWindow baseWin)
        {
            UpdateButtons(baseWin);
        }

        private void UpdateButtons(IBaseWindow baseWin)
        {
            if (baseWin != null) {
                var recType = baseWin.GetSelectedRecordType();
                var hasFS = FoldersHelper.HasFolderSupport(recType);
                var selectedRecord = baseWin.GetSelectedRecordEx();

                int treeRecords = fBase.Context.Tree.RecordsCount;
                int selRecords = fBase.GetContentList(recType).Count;

                btnSetFilter.Enabled = hasFS;
                btnSetCurrent.Enabled = hasFS && (selectedRecord != null);
                btnSetSelected.Enabled = hasFS && (selRecords > 0);
                btnSetAll.Enabled = hasFS && (treeRecords > 0);
            }
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_MIFolders);
        }

        #endregion
    }
}
