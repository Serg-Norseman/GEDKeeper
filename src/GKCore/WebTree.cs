using System;
using System.IO;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public class WebTree
	{
		private enum TCellKind : byte { ckSpace, ckLine, ckPerson }

		private class TTreeCell
		{
			public string Name;
			public TCellKind Kind;
			public int ColIndex;
			public TList Row;
			public TGEDCOMIndividualRecord Rec;
		}

		public WebTree()
		{
		}

		private TTreeCell AddCell(TList aRow, TGEDCOMIndividualRecord iRec, TCellKind aKind)
		{
			TTreeCell Result = new TTreeCell();
			Result.ColIndex = aRow.Add(Result);
			Result.Kind = aKind;
			Result.Rec = iRec;
			Result.Row = aRow;
			if (iRec != null) Result.Name = iRec.aux_GetNameStr(true, false);
			return Result;
		}

		private TList AddRow(TList aTable, int aRowIndex, int aSpaces)
		{
			TList Result = new TList(true);
			aTable.Insert(aRowIndex, Result);
			return Result;
		}

		private void DrawLine(TList aTable, TTreeCell cell_ancestor, TTreeCell cell_descendant)
		{
			int r = aTable.IndexOf(cell_descendant.Row);
			int r2 = aTable.IndexOf(cell_ancestor.Row);
			int y;
			if (r > r2)
			{
				y = r2;
				r2 = r;
				r = y;
			}

			int x = cell_descendant.ColIndex - 1;
			for (y = r; y <= r2; y++)
			{
				TList row = aTable[y] as TList;
				(row[x] as TTreeCell).Kind = TCellKind.ckLine;
			}
		}

		private void Step(TList aTable, int row_index, int col_index, TTreeCell prev, TGEDCOMIndividualRecord cur, int gen)
		{
			if (cur != null)
			{
				if (row_index < 0) row_index = 0;
				if (row_index > aTable.Count) row_index = aTable.Count;

				TList row = this.AddRow(aTable, row_index, 0);

				int num = (col_index - 1) << 1;
				for (int i = 0; i <= num; i++) this.AddCell(row, null, TCellKind.ckSpace);

				if (prev != null) this.AddCell(row, null, TCellKind.ckLine);

				TTreeCell cur_cell = this.AddCell(row, cur, TCellKind.ckPerson);
				if (cur.ChildToFamilyLinks.Count > 0 && gen < 5)
				{
					TGEDCOMFamilyRecord family = cur.ChildToFamilyLinks[0].Family;
					TGEDCOMIndividualRecord iFather = family.Husband.Value as TGEDCOMIndividualRecord;
					TGEDCOMIndividualRecord iMother = family.Wife.Value as TGEDCOMIndividualRecord;
					if (iFather != null || iMother != null)
					{
						this.AddCell(row, null, TCellKind.ckLine);
						this.AddCell(row, null, TCellKind.ckSpace);

						row_index = aTable.IndexOf(row);
						if (iFather != null) this.AddRow(aTable, row_index, col_index + 1);
						this.Step(aTable, row_index, col_index + 1, cur_cell, iFather, gen + 1);

						row_index = aTable.IndexOf(row);
						if (iMother != null) this.AddRow(aTable, row_index + 1, col_index + 1);
						this.Step(aTable, row_index + 2, col_index + 1, cur_cell, iMother, gen + 1);
					}
				}

				this.WideTable(aTable, cur_cell.ColIndex + 1);
				if (prev != null) this.DrawLine(aTable, prev, cur_cell);
			}
		}

		private void WideTable(TList aTable, int cols)
		{
			for (int i = 0; i <= aTable.Count - 1; i++) {
				TList row = aTable[i] as TList;
				while (row.Count < cols) {
					this.AddCell(row, null, TCellKind.ckSpace);
				}
			}
		}

		public void GenTree(StreamWriter aStream, TGEDCOMIndividualRecord iRec)
		{
			try
			{
				TList table_rows = new TList(true);
				try
				{
					this.Step(table_rows, 0, 0, null, iRec, 1);

					aStream.WriteLine("<table border=\"0\" cellspacing=\"0\">");

					int num = table_rows.Count - 1;
					for (int r = 0; r <= num; r++) {
						TList row = table_rows[r] as TList;
						aStream.WriteLine("<tr>");

						int num2 = row.Count - 1;
						for (int c = 0; c <= num2; c++) {
							TTreeCell cell = row[c] as TTreeCell;
							string nm = "&nbsp;";
							string st = "";
							if (cell.Kind != TCellKind.ckSpace) {
								if (cell.Kind == TCellKind.ckPerson && cell.Name != "") {
									nm = "<a href=\"#" + cell.Rec.XRef + "\">" + cell.Name + "</a>";
								}
								st = " bgcolor=\"silver\"";
							}
							aStream.WriteLine("<td" + st + ">" + nm + "</td>");
						}
						aStream.WriteLine("</tr>");
					}
					aStream.WriteLine("</tr></table>");
				}
				finally
				{
					table_rows.Dispose();
				}
			}
			catch (Exception E)
			{
				aStream.WriteLine(E.Message);
			}
		}

	}
}
