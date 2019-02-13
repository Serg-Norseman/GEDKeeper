/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKFoldersPlugin
{
    public partial class FoldersWidget : Form, ILocalization
    {
        private readonly Plugin fPlugin;

        private IBaseWindow fBase;
        private StringList fFolders;
        private string fFilterFolder;

        public FoldersWidget(Plugin plugin)
        {
            InitializeComponent();

            Location = new Point(10, Screen.PrimaryScreen.WorkingArea.Height - Height - 10);

            fPlugin = plugin;
            fFolders = new StringList();
            fFolders.DuplicateSolve = DuplicateSolve.Ignore;
            fFolders.Sorted = true;
            fFilterFolder = string.Empty;

            SetLang();
        }

        private void FoldersWidget_Load(object sender, EventArgs e)
        {
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
                GEDCOMRecord rec = fBase.Context.Tree[i];
                if (rec.HasFolderSupport()) {
                    string folder = rec.GetFolder();
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
        }

        private bool FilterHandler(GEDCOMRecord record)
        {
            bool result = true;
            try {
                if (!string.IsNullOrEmpty(fFilterFolder) && record.HasFolderSupport()) {
                    result = fFilterFolder.Equals(record.GetFolder());
                }
            } catch (Exception ex) {
                Logger.LogWrite("FoldersWidget.FilterHandler(): " + ex.Message);
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
                if (record != null) {
                    record.SetFolder(folder);
                }
            }

            fBase.Context.Modified = true;
            CollectData();
            UpdateControls();
        }

        private void btnSetSelected_Click(object sender, EventArgs e)
        {
            
        }

        private void btnSetAll_Click(object sender, EventArgs e)
        {
            string folder = cmbSelectFolder.Text;
            if (fBase != null) {
                var tree = fBase.Context.Tree;

                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GEDCOMRecord rec = tree[i];
                    if (rec.HasFolderSupport()) {
                        rec.SetFolder(folder);
                    }
                }
            }

            fBase.Context.Modified = true;
            CollectData();
            UpdateControls();
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_MIFolders);

            //StatusUpdate();
        }

        #endregion
    }
}
