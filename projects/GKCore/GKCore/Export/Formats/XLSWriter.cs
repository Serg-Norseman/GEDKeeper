/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Graphics;

namespace GKCore.Export.Formats
{
#if !NETCOREAPP && !NETSTANDARD2_0
    using ExcelLibrary.SpreadSheet;
#else
    using SwiftExcel;
#endif

    /// <summary>
    /// 
    /// </summary>
    public class XLSWriter : TableWriter
    {
        private int fColumnsCount;
        private int fTableCol;
        private int fTableRow;

        public XLSWriter()
        {
        }

        public override void BeginTable(int columnsCount, int rowsCount)
        {
            fColumnsCount = columnsCount;
            fTableRow = 1;
            fTableCol = 1;
        }

        private void NextCellAndCheckEOL()
        {
            fTableCol += 1;
            if (fTableCol > fColumnsCount) {
                fTableRow += 1;
                fTableCol = 1;
            }
        }

#if !NETCOREAPP && !NETSTANDARD2_0

        private Workbook fWorkbook;
        private Worksheet fWorksheet;

        public override void BeginWrite()
        {
            fWorkbook = new Workbook();
            fWorksheet = new Worksheet("First Sheet");
        }

        public override void EndWrite()
        {
            fWorkbook.Worksheets.Add(fWorksheet);
            fWorkbook.Save(fFileName);
        }

        public override void AddTableCell(string content, IFont font = null, TextAlignment alignment = TextAlignment.taLeft)
        {
            if (!string.IsNullOrEmpty(content)) {
                fWorksheet.Cells[fTableRow, fTableCol] = new Cell(content);
            }
            NextCellAndCheckEOL();
        }

#else

        private ExcelWriter fWorkbook;

        public override void BeginWrite()
        {
            fWorkbook = new ExcelWriter(fFileName);
        }

        public override void EndWrite()
        {
            fWorkbook.Save();
        }

        public override void AddTableCell(string content, IFont font = null, TextAlignment alignment = TextAlignment.taLeft)
        {
            if (!string.IsNullOrEmpty(content)) {
                fWorkbook.Write(content, fTableCol, fTableRow);
            }
            NextCellAndCheckEOL();
        }
#endif
    }
}
