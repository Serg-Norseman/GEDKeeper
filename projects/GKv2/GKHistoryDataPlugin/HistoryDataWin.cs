/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using Externals;
using GKCommon;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKHistoryDataPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class HistoryDataWin : Form, ILocalization, IWidgetForm
    {
        private readonly Plugin fPlugin;
        private int fLinkColumn;

        public HistoryDataWin(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;

            Screen scr = Screen.PrimaryScreen;
            Location = new Point(scr.WorkingArea.Width - Width - 10, scr.WorkingArea.Height - Height - 10);

            LoadData();

            SetLang();
        }

        private void HistoryDataWin_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
        }

        private void HistoryDataWin_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private void LoadData()
        {
            string csvPath = AppHost.GetAppPath() + "externals/sources-ru.csv";

            fLinkColumn = -1;
            using (var csv = CSVReader.CreateFromFile(csvPath)) {
                var row = csv.ReadRow();
                for (int i = 0; i < row.Count; i++) {
                    bool autoSize = (i == 0);
                    lvData.AddColumn(row[i].ToString(), 200, autoSize);
                }

                row = csv.ReadRow();
                while (row != null) {
                    if (fLinkColumn == -1) {
                        for (int i = 0; i < row.Count; i++) {
                            if (row[i].ToString().StartsWith("http")) {
                                fLinkColumn = i;
                                break;
                            }
                        }
                    }

                    lvData.AddItem(null, row.ToArray());

                    row = csv.ReadRow();
                }
            }
        }

        private void lvData_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            GKListItem item = lvData.GetSelectedItem();
            if (item != null && fLinkColumn != -1) {
                SysUtils.LoadExtFile(item.SubItems[fLinkColumn].Text);
            }
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(HDLS.LSID_Title);
        }

        #endregion
    }
}

