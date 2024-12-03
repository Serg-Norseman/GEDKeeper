/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.NetDiff;
using GKCore.Tools;

namespace GKTreeSyncPlugin
{
    public partial class TSForm : Form, ILocalizable
    {
        private readonly IBaseWindow fBase;
        private readonly SyncTool fSyncTool;

        public TSForm()
        {
            InitializeComponent();
        }

        public TSForm(Plugin plugin, IBaseWindow curBase) : this()
        {
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                var name = LangMan.LS(GKData.RecordTypes[(int)rt].Name);
                cmbRecordTypes.Items.Add(name);
            }

            fBase = curBase;
            fSyncTool = new SyncTool();

            lvRecords.CheckBoxes = true;
            lvRecords.AddColumn("XRef 1", 100);
            lvRecords.AddColumn("XRef 2", 100);
            lvRecords.AddColumn("Name 1", 400);
            lvRecords.AddColumn("Name 2", 400);
        }

        public void SetLocale()
        {
        }

        private async void btnSelectFile_ClickAsync(object sender, EventArgs e)
        {
            string fileName = await AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) {
                txtFile.Text = string.Empty;
                return;
            }

            txtFile.Text = fileName;
            fSyncTool.LoadOtherFile(fBase.Context.Tree, fileName);
            fSyncTool.CompareRecords(GetRecordType());
            UpdateLists();
        }

        private void rbSyncRecords_CheckedChanged(object sender, EventArgs e)
        {
            cmbRecordTypes.Enabled = !rbSyncAll.Checked;
        }

        private void chkOnlyModified_CheckStateChanged(object sender, EventArgs e)
        {
            UpdateLists();
        }

        private GDMRecordType GetRecordType()
        {
            if (rbSyncAll.Checked) {
                return GDMRecordType.rtNone;
            } else {
                return (GDMRecordType)(cmbRecordTypes.SelectedIndex + 1);
            }
        }

        private void UpdateLists()
        {
            bool onlyModified = chkOnlyModified.Checked;

            lvRecords.BeginUpdate();
            lvRecords.ClearItems();

            var tree = fBase.Context.Tree;
            for (int i = 0; i < fSyncTool.Results.Count; i++) {
                var compRes = fSyncTool.Results[i];
                if (onlyModified && compRes.Status == DiffStatus.Equal) continue;

                string item1, item2;
                char diffChar = DiffUtil.GetStatusChar(compRes.Status);
                Color backColor;

                switch (compRes.Status) {
                    case DiffStatus.Equal:
                    default:
                        item1 = diffChar + " " + compRes.Obj1.XRef;
                        item2 = diffChar + " " + compRes.Obj2.XRef;
                        backColor = Color.White;
                        break;

                    case DiffStatus.Deleted:
                        item1 = diffChar + " " + compRes.Obj1.XRef;
                        item2 = " ";
                        backColor = Color.Coral;
                        break;

                    case DiffStatus.Inserted:
                        item1 = " ";
                        item2 = diffChar + " " + compRes.Obj2.XRef;
                        backColor = Color.LightBlue;
                        break;

                    case DiffStatus.Modified:
                    case DiffStatus.DeepModified:
                        item1 = diffChar + " " + compRes.Obj1.XRef;
                        item2 = diffChar + " " + compRes.Obj2.XRef;
                        backColor = (compRes.Status == DiffStatus.Modified) ? Color.Yellow : Color.Orange;
                        break;
                }

                lvRecords.AddItem(compRes, false, backColor,
                    item1, item2,
                    GKUtils.GetRecordName(tree, compRes.Obj1, false), GKUtils.GetRecordName(tree, compRes.Obj2, false));
            }

            lvRecords.EndUpdate();
        }
    }
}
