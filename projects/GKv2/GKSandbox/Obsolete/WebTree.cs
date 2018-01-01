/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2011 by Sergey V. Zhdanovskih.
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
using System.IO;
using BSLib;
using GKCommon.GEDCOM;

namespace GKCore
{
    public class WebTree
    {
        private enum CellKind
        {
            ckSpace,
            ckLine,
            ckPerson

        }

        private class TreeCell
        {
            public string Name;
            public CellKind Kind;
            public int ColIndex;
            public ExtList<TreeCell> Row;
            public GEDCOMIndividualRecord Rec;
        }

        public WebTree()
        {
        }

        private static TreeCell AddCell(ExtList<TreeCell> row, GEDCOMIndividualRecord iRec, CellKind cellKind)
        {
            TreeCell result = new TreeCell();
            result.ColIndex = row.Add(result);
            result.Kind = cellKind;
            result.Rec = iRec;
            result.Row = row;
            if (iRec != null)
                result.Name = iRec.GetPrimaryFullName();
            return result;
        }

        private static ExtList<TreeCell> AddRow(ExtList<ExtList<TreeCell>> table, int rowIndex, int spaces)
        {
            ExtList<TreeCell> result = new ExtList<TreeCell>(true);
            table.Insert(rowIndex, result);
            return result;
        }

        private static void DrawLine(ExtList<ExtList<TreeCell>> table, TreeCell cellAncestor, TreeCell cellDescendant)
        {
            int r = table.IndexOf(cellDescendant.Row);
            int r2 = table.IndexOf(cellAncestor.Row);
            int y;
            if (r > r2) {
                y = r2;
                r2 = r;
                r = y;
            }

            int x = cellDescendant.ColIndex - 1;
            for (y = r; y <= r2; y++) {
                var row = table[y];
                row[x].Kind = CellKind.ckLine;
            }
        }

        private void Step(ExtList<ExtList<TreeCell>> table, int rowIndex, int colIndex,
            TreeCell prev, GEDCOMIndividualRecord cur, int gen)
        {
            if (cur == null)
                return;

            if (rowIndex < 0)
                rowIndex = 0;
            if (rowIndex > table.Count)
                rowIndex = table.Count;

            var row = AddRow(table, rowIndex, 0);

            int num = (colIndex - 1) << 1;
            for (int i = 0; i <= num; i++)
                AddCell(row, null, CellKind.ckSpace);

            if (prev != null)
                AddCell(row, null, CellKind.ckLine);

            TreeCell curCell = AddCell(row, cur, CellKind.ckPerson);
            if (cur.ChildToFamilyLinks.Count > 0 && gen < 5) {
                GEDCOMFamilyRecord family = cur.ChildToFamilyLinks[0].Family;
                GEDCOMIndividualRecord iFather = family.Husband.Value as GEDCOMIndividualRecord;
                GEDCOMIndividualRecord iMother = family.Wife.Value as GEDCOMIndividualRecord;
                if (iFather != null || iMother != null) {
                    AddCell(row, null, CellKind.ckLine);
                    AddCell(row, null, CellKind.ckSpace);

                    rowIndex = table.IndexOf(row);
                    if (iFather != null)
                        AddRow(table, rowIndex, colIndex + 1);
                    Step(table, rowIndex, colIndex + 1, curCell, iFather, gen + 1);

                    rowIndex = table.IndexOf(row);
                    if (iMother != null)
                        AddRow(table, rowIndex + 1, colIndex + 1);
                    Step(table, rowIndex + 2, colIndex + 1, curCell, iMother, gen + 1);
                }
            }

            WideTable(table, curCell.ColIndex + 1);
            if (prev != null)
                DrawLine(table, prev, curCell);
        }

        private void WideTable(ExtList<ExtList<TreeCell>> table, int cols)
        {
            for (int i = 0; i < table.Count; i++) {
                var row = table[i];
                while (row.Count < cols) {
                    AddCell(row, null, CellKind.ckSpace);
                }
            }
        }

        public void GenTree(StreamWriter writer, GEDCOMIndividualRecord iRec)
        {
            try {
                var tableRows = new ExtList<ExtList<TreeCell>>(true);
                try {
                    Step(tableRows, 0, 0, null, iRec, 1);

                    writer.WriteLine("<table border=\"0\" cellspacing=\"0\">");

                    int num = tableRows.Count;
                    for (int r = 0; r < num; r++) {
                        var row = tableRows[r];
                        writer.WriteLine("<tr>");

                        int num2 = row.Count;
                        for (int c = 0; c < num2; c++) {
                            TreeCell cell = row[c];
                            string nm = "&nbsp;";
                            string st = "";
                            if (cell.Kind != CellKind.ckSpace) {
                                if (cell.Kind == CellKind.ckPerson && cell.Name != "") {
                                    nm = "<a href=\"#" + cell.Rec.XRef + "\">" + cell.Name + "</a>";
                                }
                                st = " bgcolor=\"silver\"";
                            }
                            writer.WriteLine("<td" + st + ">" + nm + "</td>");
                        }
                        writer.WriteLine("</tr>");
                    }
                    writer.WriteLine("</tr></table>");
                } finally {
                    tableRows.Dispose();
                }
            } catch (Exception ex) {
                writer.WriteLine(ex.Message);
            }
        }
    }
}
