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

using GKCore.Design.Graphics;

namespace GKCore.Export
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
