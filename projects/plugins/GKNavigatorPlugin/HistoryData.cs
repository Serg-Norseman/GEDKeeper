
/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using BSLib;
using GKCore;
using BSDListItem = GKCore.Design.Controls.IListItem;

namespace GKNavigatorPlugin
{
    internal enum LinkState
    {
        Normal,
        Invalid,
        Duplicate
    }


    internal class LinkItem
    {
        public object[] Data;
        public BSDListItem Item;
        public LinkState State;

        public LinkItem(object[] data)
        {
            Data = data;
        }
    }


    internal class HistoryData
    {
        private readonly List<LinkItem> fItems;
        private int fLinkColumn;

        public string[] CSVFiles { get; private set; }

        public object[] Headers { get; private set; }

        public List<LinkItem> Items
        {
            get { return fItems; }
        }

        public int LinkColumn
        {
            get { return fLinkColumn; }
        }


        public HistoryData()
        {
            fItems = new List<LinkItem>();
            LoadFiles();
        }

        private void LoadFiles()
        {
            try {
                string csvPath = AppHost.GetAppPath() + "externals/";
                if (!Directory.Exists(csvPath)) {
                    Directory.CreateDirectory(csvPath);
                }
                CSVFiles = Directory.GetFiles(csvPath, "*.csv");
            } catch (Exception ex) {
                Logger.WriteError("HistoryData.LoadFiles()", ex);
                CSVFiles = new string[0];
            }
        }

        public bool LoadDataFile(string fileName)
        {
            string csvPath = AppHost.GetAppPath() + "externals/" + fileName;
            if (!File.Exists(csvPath)) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileNotFound, fileName));
                return false;
            }

            fItems.Clear();
            fLinkColumn = -1;
            int rowNum = 0;
            using (var csv = CSVReader.CreateFromFile(csvPath, Encoding.UTF8)) {
                var row = csv.ReadRow();
                while (row != null) {
                    var data = row.ToArray();

                    if (rowNum == 0) {
                        Headers = data;
                    } else {
                        if (fLinkColumn == -1) {
                            fLinkColumn = CheckLinkColumn(row);
                        }

                        fItems.Add(new LinkItem(row.ToArray()));
                    }

                    row = csv.ReadRow();
                    rowNum += 1;
                }
            }

            return true;
        }

        public static int CheckLinkColumn(List<object> row)
        {
            for (int i = 0; i < row.Count; i++) {
                if (row[i].ToString().StartsWith("http")) {
                    return i;
                }
            }
            return -1;
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

        public void Check()
        {
            try {
                Thread worker = new Thread(WorkerMethod);
#if OS_MSWIN
                worker.SetApartmentState(ApartmentState.STA);
#endif
                worker.IsBackground = true;
                worker.Start();
            } catch (Exception ex) {
                Logger.WriteError("HistoryData.Check()", ex);
            }
        }

        private void WorkerMethod()
        {
            try {
                int num = fItems.Count;
                for (int i = 0; i < num; i++) {
                    var linkItem1 = fItems[i];
                    string url1 = linkItem1.Data[fLinkColumn].ToString();

                    for (int k = i + 1; k < num; k++) {
                        var linkItem2 = fItems[i];
                        string url2 = linkItem2.Data[fLinkColumn].ToString();

                        if (string.Compare(url1, url2, true) == 0) {
                            linkItem2.State = LinkState.Duplicate;
                        }
                    }

                    if (!IsValidUrl(url1)) {
                        linkItem1.State = LinkState.Invalid;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("HistoryData.WorkerMethod()", ex);
            }
        }
    }
}
