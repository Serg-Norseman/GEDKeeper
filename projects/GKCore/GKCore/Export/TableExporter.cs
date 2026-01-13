/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Export.Formats;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TableExporter : Exporter
    {
        private readonly GDMRecordType fRecType;
        private readonly IRecordsListModel fListMan;

        public TableExporter(IBaseWindow baseWin) : base(baseWin)
        {
            fRecType = fBase.GetSelectedRecordType();
            fListMan = fBase.GetRecordsListManByType(fRecType);
        }

        private void GenerateInt(IProgressController progress)
        {
            int recordsCount = fListMan.FilteredCount;
            progress.Begin(LangMan.LS(LSID.MIExport) + "...", recordsCount);
            try {
                var columns = fListMan.ColumnsMap;
                int colNum = columns.Count;
                fWriter.BeginTable(colNum, recordsCount + 1);

                for (int k = 0; k < colNum; k++) {
                    fWriter.AddTableCell(columns[k].Caption);
                }

                for (int i = 0; i < recordsCount; i++) {
                    object rowData = fListMan.GetContentItem(i);
                    var itemData = fListMan.GetItemData(rowData);
                    for (int k = 0; k < itemData.Length; k++) {
                        string colVal = itemData[k].ToString();
                        colVal = colVal.Replace("\r\n", " ").Replace("\"", "'");
                        fWriter.AddTableCell(colVal);
                    }

                    progress.Increment();
                }
            } finally {
                progress.End();
            }
        }

        public override async void Generate(bool show)
        {
            fPath = await GetTableFile();
            fWriter = GetTableWriter(fPath);
            if (fWriter == null) return;

            fWriter.BeginWrite();
            AppHost.Instance.ExecuteWork((controller) => {
                GenerateInt(controller);
            });
            fWriter.EndWrite();

#if !CI_MODE
            if (show) ShowResult();
#endif
        }

        public static async Task<string> GetTableFile()
        {
            string availableFormats = "CSV files (*.csv)|*.csv";
#if !NETCOREAPP
            availableFormats += "|" + "Excel files (*.xls)|*.xls";
#else
            availableFormats += "|" + "Excel files (*.xlsx)|*.xlsx";
#endif

            return await AppHost.StdDialogs.GetSaveFile(GlobalOptions.Instance.ReportExportLastDir, availableFormats);
        }

        /// <summary>
        /// Returns the writer of the table, given the availability of the output method and the selected filename.
        /// </summary>
        /// <returns></returns>
        public static TableWriter GetTableWriter(string fileName)
        {
            if (string.IsNullOrEmpty(fileName)) return null;

            GlobalOptions.Instance.ReportExportLastDir = Path.GetDirectoryName(fileName);

            TableWriter result;

            string ext = FileHelper.GetFileExtension(fileName);
            if (string.Equals(ext, ".xls") || string.Equals(ext, ".xlsx")) {
                result = new XLSWriter();
            } else {
                result = new CSVWriter();
            }

            result.SetFileName(fileName);

            return result;
        }
    }
}
