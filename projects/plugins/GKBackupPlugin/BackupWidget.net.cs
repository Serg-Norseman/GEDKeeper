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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Plugins;

namespace GKBackupPlugin
{
    public partial class BackupWidget : Form, IWidgetForm
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private CheckBox chkEnabled;
        private Label lblFolder;
        private TextBox txtFolder;
        private Button btnFolderChoose;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly Plugin fPlugin;

        public BackupWidget(Plugin plugin)
        {
            XamlReader.Load(this);

            fPlugin = plugin;

            SetLocale();
        }

        private void Form_Shown(object sender, EventArgs e)
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
            if (chkEnabled.HasFocus) {
                fPlugin.ExtendedBackupEnabled = chkEnabled.Checked.Value;
            }
            btnFolderChoose.Enabled = chkEnabled.Checked.Value;
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
            Title = fPlugin.LangMan.LS(PLS.Backup);
            chkEnabled.Text = fPlugin.LangMan.LS(PLS.Enabled);
            lblFolder.Text = fPlugin.LangMan.LS(PLS.Folder);
            btnFolderChoose.Text = fPlugin.LangMan.LS(PLS.FolderChoose);
        }
    }
}
