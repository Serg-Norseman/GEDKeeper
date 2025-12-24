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

using System.IO;
using System.Text;
using GKCore.Design.Graphics;

namespace GKCore.Export.Formats
{
    /// <summary>
    /// 
    /// </summary>
    public class CSVWriter : TableWriter
    {
        private int fColumnsCount;
        private int fTableCol;
        private int fTableRow;
        private StreamWriter fStream;

        public CSVWriter()
        {
        }

        public override void BeginWrite()
        {
            fStream = new StreamWriter(new FileStream(fFileName, FileMode.Create, FileAccess.Write), Encoding.UTF8);
        }

        public override void EndWrite()
        {
            fStream.Flush();
            fStream.Close();
        }

        public override void BeginTable(int columnsCount, int rowsCount)
        {
            fColumnsCount = columnsCount;
            fTableRow = 1;
            fTableCol = 1;
        }

        public override void AddTableCell(string content, IFont font = null, TextAlignment alignment = TextAlignment.taLeft)
        {
            if (fTableCol > 1) {
                fStream.Write(";");
            }

            if (!string.IsNullOrEmpty(content)) {
                fStream.Write(string.Concat("\"", content, "\""));
            }

            fTableCol += 1;
            if (fTableCol > fColumnsCount) {
                fStream.WriteLine();
                fTableRow += 1;
                fTableCol = 1;
            }
        }
    }
}
