/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#pragma warning disable CA1416

using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using BSLib;
using GKCore;
using GKCore.Locales;
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


    internal class WebLinksData
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


        public WebLinksData()
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

        public async void Check()
        {
            await Task.Run(() => {
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
                    Logger.WriteError("HistoryData.Check()", ex);
                }
            });
        }
    }
}
