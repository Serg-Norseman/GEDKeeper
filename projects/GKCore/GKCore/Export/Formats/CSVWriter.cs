/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
