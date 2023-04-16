/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Interfaces;

namespace GKBackupPlugin
{
    public partial class BackupWidget : Form, IWidgetForm
    {
        private readonly Plugin fPlugin;

        public BackupWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;

            SetLocale();
        }

        private void Form_Load(object sender, EventArgs e)
        {
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VBottom);
            fPlugin.Host.WidgetShow(fPlugin);

            chkEnabled.Checked = fPlugin.ExtendedBackupEnabled;
            txtFolder.Text = fPlugin.Folder;
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private void chkEnabled_CheckedChanged(object sender, EventArgs e)
        {
            if (chkEnabled.Focused) {
                fPlugin.ExtendedBackupEnabled = chkEnabled.Checked;
            }
            btnFolderChoose.Enabled = chkEnabled.Checked;
        }

        private void btnFolderChoose_Click(object sender, EventArgs e)
        {
            var selectedFolder = AppHost.Instance.SelectFolder(fPlugin.Folder);
            if (!string.IsNullOrEmpty(selectedFolder)) {
                fPlugin.Folder = selectedFolder;
                txtFolder.Text = fPlugin.Folder;
            }
        }

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_Backup);
            chkEnabled.Text = fPlugin.LangMan.LS(PLS.LSID_Enabled);
            lblFolder.Text = fPlugin.LangMan.LS(PLS.LSID_Folder);
            btnFolderChoose.Text = fPlugin.LangMan.LS(PLS.LSID_FolderChoose);
        }
    }
}
