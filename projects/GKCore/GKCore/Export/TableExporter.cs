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
using GKCore.Design;
using GKCore.Export.Formats;
using GKCore.Options;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class TableExporter : Exporter
    {
        public TableExporter(IBaseWindow baseWin) : base(baseWin)
        {
        }

        protected virtual void GenerateInt(IProgressController progress)
        {
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
#if !TERM
#if !NETCOREAPP
            availableFormats += "|" + "Excel files (*.xls)|*.xls";
#else
            availableFormats += "|" + "Excel files (*.xlsx)|*.xlsx";
#endif
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
#if !TERM
            if (string.Equals(ext, ".xls") || string.Equals(ext, ".xlsx")) {
                result = new XLSWriter();
            } else
#endif
            {
                result = new CSVWriter();
            }

            result.SetFileName(fileName);

            return result;
        }
    }
}
