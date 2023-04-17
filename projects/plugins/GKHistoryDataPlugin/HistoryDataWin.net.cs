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
using System.IO;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;
using GKUI.Platform;

namespace GKHistoryDataPlugin
{
    public partial class HistoryDataWin : Form, IWidgetForm
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private ToolBar toolStrip1;
        private ButtonToolItem btnCheck;
        private ButtonToolItem btnLoadFile;
        private GKDropDownToolItem cbDataFiles;
        private ContextMenu mnuFiles;
        private GKListView lvData;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly HistoryData fData;
        private readonly Plugin fPlugin;

        public HistoryDataWin(Plugin plugin)
        {
            XamlReader.Load(this);

            fPlugin = plugin;
            fData = new HistoryData();
            UpdateFiles();

            SetLocale();
        }

        public void SetLocale()
        {
            Title = fPlugin.LangMan.LS(HDLS.LSID_Title);
        }

        private void Form_Shown(object sender, EventArgs e)
        {
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VBottom);
            fPlugin.Host.WidgetShow(fPlugin);
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private void UpdateFiles()
        {
            mnuFiles.Items.Clear();
            foreach (string cf in fData.CSVFiles) {
                var mi = new ButtonMenuItem();
                mi.Text = Path.GetFileName(cf);
                mi.Click += (sender, e) => {
                    cbDataFiles.Text = ((ButtonMenuItem)sender).Text;
                };

                mnuFiles.Items.Add(mi);
            }
        }

        private void lvData_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            var item = lvData.GetSelectedData() as LinkItem;
            if (item != null && fData.LinkColumn != -1) {
                GKUtils.LoadExtFile(item.Data[fData.LinkColumn].ToString());
            }
        }

        private void btnCheck_Click(object sender, EventArgs e)
        {
            fData.Check();
            RefreshList();
        }

        private void RefreshList()
        {
            lvData.Clear();

            for (int i = 0; i < fData.Headers.Length; i++) {
                bool autoSize = (i == 0);
                lvData.AddColumn(fData.Headers[i].ToString(), 200, autoSize);
            }

            for (int i = 0; i < fData.Items.Count; i++) {
                var item = fData.Items[i];
                item.Item = lvData.AddItem(item, item.Data);
                var listItem = item.Item as GKListItem;

                switch (item.State) {
                    case LinkState.Normal:
                        listItem.BackColor = Colors.PaleGreen;
                        break;
                    case LinkState.Invalid:
                        listItem.BackColor = Colors.IndianRed;
                        break;
                    case LinkState.Duplicate:
                        listItem.BackColor = Colors.Orange;
                        break;
                }
            }
        }

        private void btnLoadFile_Click(object sender, EventArgs e)
        {
            if (fData.LoadDataFile(cbDataFiles.Text)) {
                RefreshList();
            }
        }
    }
}
