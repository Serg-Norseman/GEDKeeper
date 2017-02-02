using System;
using System.IO;

using GKCommon;
using GKCommon.GEDCOM;

namespace GKCore.Export
{
    public class WebTree
    {
        private enum CellKind { ckSpace, ckLine, ckPerson }

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

        private TreeCell AddCell(ExtList<TreeCell> row, GEDCOMIndividualRecord iRec, CellKind cellKind)
        {
            TreeCell result = new TreeCell();
            result.ColIndex = row.Add(result);
            result.Kind = cellKind;
            result.Rec = iRec;
            result.Row = row;
            if (iRec != null) result.Name = iRec.GetPrimaryFullName();
            return result;
        }

        private ExtList<TreeCell> AddRow(ExtList<ExtList<TreeCell>> table, int rowIndex, int spaces)
        {
            ExtList<TreeCell> result = new ExtList<TreeCell>(true);
            table.Insert(rowIndex, result);
            return result;
        }

        private void DrawLine(ExtList<ExtList<TreeCell>> table, TreeCell cellAncestor, TreeCell cellDescendant)
        {
            int r = table.IndexOf(cellDescendant.Row);
            int r2 = table.IndexOf(cellAncestor.Row);
            int y;
            if (r > r2)
            {
                y = r2;
                r2 = r;
                r = y;
            }

            int x = cellDescendant.ColIndex - 1;
            for (y = r; y <= r2; y++)
            {
                var row = table[y];
                row[x].Kind = CellKind.ckLine;
            }
        }

        private void Step(ExtList<ExtList<TreeCell>> table, int rowIndex, int colIndex,
                          TreeCell prev, GEDCOMIndividualRecord cur, int gen)
        {
            if (cur != null)
            {
                if (rowIndex < 0) rowIndex = 0;
                if (rowIndex > table.Count) rowIndex = table.Count;

                var row = this.AddRow(table, rowIndex, 0);

                int num = (colIndex - 1) << 1;
                for (int i = 0; i <= num; i++) this.AddCell(row, null, CellKind.ckSpace);

                if (prev != null) this.AddCell(row, null, CellKind.ckLine);

                TreeCell cur_cell = this.AddCell(row, cur, CellKind.ckPerson);
                if (cur.ChildToFamilyLinks.Count > 0 && gen < 5)
                {
                    GEDCOMFamilyRecord family = cur.ChildToFamilyLinks[0].Family;
                    GEDCOMIndividualRecord iFather = family.Husband.Value as GEDCOMIndividualRecord;
                    GEDCOMIndividualRecord iMother = family.Wife.Value as GEDCOMIndividualRecord;
                    if (iFather != null || iMother != null)
                    {
                        this.AddCell(row, null, CellKind.ckLine);
                        this.AddCell(row, null, CellKind.ckSpace);

                        rowIndex = table.IndexOf(row);
                        if (iFather != null) this.AddRow(table, rowIndex, colIndex + 1);
                        this.Step(table, rowIndex, colIndex + 1, cur_cell, iFather, gen + 1);

                        rowIndex = table.IndexOf(row);
                        if (iMother != null) this.AddRow(table, rowIndex + 1, colIndex + 1);
                        this.Step(table, rowIndex + 2, colIndex + 1, cur_cell, iMother, gen + 1);
                    }
                }

                this.WideTable(table, cur_cell.ColIndex + 1);
                if (prev != null) this.DrawLine(table, prev, cur_cell);
            }
        }

        private void WideTable(ExtList<ExtList<TreeCell>> table, int cols)
        {
            for (int i = 0; i <= table.Count - 1; i++) {
                var row = table[i];
                while (row.Count < cols) {
                    this.AddCell(row, null, CellKind.ckSpace);
                }
            }
        }

        public void GenTree(StreamWriter writer, GEDCOMIndividualRecord iRec)
        {
            try
            {
                var table_rows = new ExtList<ExtList<TreeCell>>(true);
                try
                {
                    this.Step(table_rows, 0, 0, null, iRec, 1);

                    writer.WriteLine("<table border=\"0\" cellspacing=\"0\">");

                    int num = table_rows.Count - 1;
                    for (int r = 0; r <= num; r++) {
                        var row = table_rows[r];
                        writer.WriteLine("<tr>");

                        int num2 = row.Count - 1;
                        for (int c = 0; c <= num2; c++) {
                            TreeCell cell = row[c] as TreeCell;
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
                }
                finally
                {
                    table_rows.Dispose();
                }
            }
            catch (Exception E)
            {
                writer.WriteLine(E.Message);
            }
        }

    }
}
