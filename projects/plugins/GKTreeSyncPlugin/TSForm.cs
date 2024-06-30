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
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Interfaces;
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
        }

        public void SetLocale()
        {
        }

        private void TSForm_Load(object sender, EventArgs e)
        {
            ResizeSplitter();
        }

        private void ResizeSplitter()
        {
            var diff = splitContainer1.Panel1.Width - splitContainer1.Panel2.Width;
            splitContainer1.SplitterDistance -= diff / 2;
        }

        private void TSForm_Resize(object sender, EventArgs e)
        {
            ResizeSplitter();
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
        }

        private void rbSyncRecords_CheckedChanged(object sender, EventArgs e)
        {
            cmbRecordTypes.Enabled = !rbSyncAll.Checked;
        }
    }
}
