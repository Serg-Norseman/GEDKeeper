/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design;
using GKCore.Lists;
using GKCore.Locales;

namespace GKCore.Export
{
    public sealed class RecordsTableExporter : TableExporter
    {
        private readonly GDMRecordType fRecType;
        private readonly IRecordsListModel fListMan;

        public RecordsTableExporter(IBaseWindow baseWin) : base(baseWin)
        {
            fRecType = fBase.GetSelectedRecordType();
            fListMan = fBase.GetRecordsListManByType(fRecType);
        }

        protected override void GenerateInt(IProgressController progress)
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
    }
}
