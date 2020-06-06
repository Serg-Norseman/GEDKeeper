/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using System.Windows.Forms;

using BSLib;
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
        public enum LinkState
        {
            Normal,
            Invalid,
            Duplicate
        }

        private class LinkItem
        {
            public GKListItem Item;
            public LinkState State;

            public LinkItem(GKListItem item)
            {
                Item = item;
            }
        }

        private readonly Plugin fPlugin;
        private int fLinkColumn;
        private List<LinkItem> fItems;

        public HistoryDataWin(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;

            Screen scr = Screen.PrimaryScreen;
            Location = new Point(scr.WorkingArea.Width - Width - 10, scr.WorkingArea.Height - Height - 10);

            fItems = new List<LinkItem>();
            LoadFiles();

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

        private void LoadFiles()
        {
            try {
                string csvPath = AppHost.GetAppPath() + "externals/";
                if (!Directory.Exists(csvPath)) {
                    Directory.CreateDirectory(csvPath);
                }
                string[] csvFiles = Directory.GetFiles(csvPath, "*.csv");

                cbDataFiles.Items.Clear();
                foreach (string cf in csvFiles) {
                    cbDataFiles.Items.Add(Path.GetFileName(cf));
                }
            } catch (Exception ex) {
                Logger.WriteError("HistoryDataWin.LoadFiles(): ", ex);
            }
        }

        private void LoadDataFile(string fileName)
        {
            string csvPath = AppHost.GetAppPath() + "externals/" + fileName;

            fItems.Clear();
            fLinkColumn = -1;
            int rowNum = 0;
            using (var csv = CSVReader.CreateFromFile(csvPath, Encoding.UTF8)) {
                var row = csv.ReadRow();
                while (row != null) {
                    if (rowNum == 0) {
                        for (int i = 0; i < row.Count; i++) {
                            bool autoSize = (i == 0);
                            lvData.AddColumn(row[i].ToString(), 200, autoSize);
                        }
                    } else {
                        if (fLinkColumn == -1) {
                            CheckLinkColumn(row);
                        }

                        fItems.Add(new LinkItem((GKListItem)lvData.AddItem(null, row.ToArray())));
                    }

                    row = csv.ReadRow();
                    rowNum += 1;
                }
            }
        }

        private void CheckLinkColumn(List<object> row)
        {
            for (int i = 0; i < row.Count; i++) {
                if (row[i].ToString().StartsWith("http")) {
                    fLinkColumn = i;
                    break;
                }
            }
        }

        private void lvData_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            GKListItem item = lvData.GetSelectedItem();
            if (item != null && fLinkColumn != -1) {
                GKUtils.LoadExtFile(item.SubItems[fLinkColumn].Text);
            }
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(HDLS.LSID_Title);
        }

        #endregion

        private void btnCheck_Click(object sender, EventArgs e)
        {
            try {
                Thread worker = new Thread(WorkerMethod);
                worker.SetApartmentState(ApartmentState.STA);
                worker.IsBackground = true;
                worker.Start();
            } catch (Exception ex) {
                Logger.WriteError("HistoryDataWin.btnCheck_Click(): ", ex);
            }
        }

        private void btnLoadFile_Click(object sender, EventArgs e)
        {
            LoadDataFile(cbDataFiles.Text);
        }

        public static bool IsValidUrl(string url)
        {
            try {
                var request = WebRequest.Create(url);
                request.Timeout = 5000;
                request.Method = "HEAD";

                using (var response = (HttpWebResponse)request.GetResponse()) {
                    response.Close();
                    return response.StatusCode == HttpStatusCode.OK;
                }
            } catch (Exception) {
                return false;
            }
        }

        private void WorkerMethod()
        {
            try {
                int num = fItems.Count;
                for (int i = 0; i < num; i++) {
                    var linkItem1 = fItems[i];
                    string url1 = linkItem1.Item.SubItems[fLinkColumn].Text;

                    for (int k = i + 1; k < num; k++) {
                        var linkItem2 = fItems[i];
                        string url2 = linkItem2.Item.SubItems[fLinkColumn].Text;

                        if (string.Compare(url1, url2, true) == 0) {
                            linkItem2.State = LinkState.Duplicate;
                        }
                    }
                }

                for (int i = 0; i < num; i++) {
                    var linkItem = fItems[i];
                    string url = linkItem.Item.SubItems[fLinkColumn].Text;
                    if (!IsValidUrl(url)) {
                        linkItem.State = LinkState.Invalid;
                    }

                    switch (linkItem.State) {
                        case LinkState.Normal:
                            linkItem.Item.BackColor = Color.PaleGreen;
                            break;
                        case LinkState.Invalid:
                            linkItem.Item.BackColor = Color.IndianRed;
                            break;
                        case LinkState.Duplicate:
                            linkItem.Item.BackColor = Color.Orange;
                            break;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("HistoryDataWin.WorkerMethod(): ", ex);
            }
        }
    }
}
