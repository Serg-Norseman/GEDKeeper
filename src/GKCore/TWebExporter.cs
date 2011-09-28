using System;
using System.IO;
using System.Text;

using GedCom551;
using GKCore.Sys;

namespace GKCore
{
	public class TWebExporter : THTMLExporter
	{
		private enum TCellKind : byte { ckSpace, ckLine, ckPerson }

		private class TTreeCell
		{
			public string Name;
			public TCellKind Kind;
			public int ColIndex;
			public TObjectList Row;
			public TGEDCOMIndividualRecord Rec;

			public void Free()
			{
				TObjectHelper.Free(this);
			}

		}

		private int FSurnamesCount;

		private bool IsLatSym(char c)
		{
			uint ch = (uint)c;
			return (ch >= (uint)'A' && ch <= (uint)'Z');
		}

		private bool IsRusSym(char c)
		{
			uint ch = (uint)c;
			return (ch >= (uint)'А' && ch <= (uint)'Я');
		}

		private TTreeCell AddCell(TObjectList aRow, TGEDCOMIndividualRecord iRec, TCellKind aKind)
		{
			TTreeCell Result = new TTreeCell();
			Result.ColIndex = aRow.Add(Result);
			Result.Kind = aKind;
			Result.Rec = iRec;
			Result.Row = aRow;
			if (iRec != null)
			{
				Result.Name = TGenEngine.GetNameStr(iRec, true, false);
			}
			return Result;
		}

		private TObjectList AddRow(TObjectList aTable, int aRowIndex, int aSpaces)
		{
			TObjectList Result = new TObjectList();
			aTable.Insert(aRowIndex, Result);
			return Result;
		}

		private void DrawLine(TObjectList aTable, TTreeCell cell_ancestor, TTreeCell cell_descendant)
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
			int arg_3A_0 = r;
			int num = r2;
			y = arg_3A_0;
			if (num >= y)
			{
				num++;
				do
				{
					TObjectList row = aTable[y] as TObjectList;
					(row[x] as TTreeCell).Kind = TCellKind.ckLine;
					y++;
				}
				while (y != num);
			}
		}

		private void Step(TObjectList aTable, int row_index, int col_index, TTreeCell prev, TGEDCOMIndividualRecord cur, int gen)
		{
			if (cur != null)
			{
				if (row_index < 0) row_index = 0;
				if (row_index > aTable.Count) row_index = aTable.Count;

				TObjectList row = this.AddRow(aTable, row_index, 0);

				int num = col_index - 1 << 1;
				int arg_37_0 = num;
				int arg_37_1 = 0;
				num++;
				if (arg_37_0 >= arg_37_1)
				{
					do
					{
						this.AddCell(row, null, TCellKind.ckSpace);
						num--;
					}
					while (num != 0);
				}

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

		private void WideTable(TObjectList aTable, int cols)
		{
			for (int i = 0; i <= aTable.Count - 1; i++) {
				TObjectList row = aTable[i] as TObjectList;
				while (row.Count < cols) {
					this.AddCell(row, null, TCellKind.ckSpace);
				}
			}
		}

		private void GenTree(StreamWriter aStream, TGEDCOMIndividualRecord iRec)
		{
			try
			{
				TObjectList table_rows = new TObjectList(true);
				try
				{
					this.Step(table_rows, 0, 0, null, iRec, 1);

					aStream.WriteLine("<table border=\"0\" cellspacing=\"0\">");

					int num = table_rows.Count - 1;
					int r = 0;
					if (num >= r)
					{
						num++;
						do
						{
							TObjectList row = table_rows[r] as TObjectList;
							aStream.WriteLine("<tr>");

							int num2 = row.Count - 1;
							int c = 0;
							if (num2 >= c)
							{
								num2++;
								do
								{
									TTreeCell cell = row[c] as TTreeCell;
									string nm = "&nbsp;";
									string st = "";
									if (cell.Kind != TCellKind.ckSpace)
									{
										if (cell.Kind == TCellKind.ckPerson && cell.Name != "")
										{
											nm = string.Concat(new string[]
											{
												"<a href=\"#", 
												cell.Rec.XRef, 
												"\">", 
												cell.Name, 
												"</a>"
											});
										}
										st = " bgcolor=\"silver\"";
									}

									aStream.WriteLine(string.Concat(new string[]
									{
										"<td", st, ">", nm, "</td>"
									}));
									c++;
								}
								while (c != num2);
							}
							aStream.WriteLine("</tr>");
							r++;
						}
						while (r != num);
					}
					aStream.WriteLine("</tr></table>");
				}
				finally
				{
					table_rows.Free();
				}
			}
			catch (Exception E)
			{
				aStream.WriteLine(E.Message);
			}
		}

		private void WritePersons()
		{
			StreamWriter fs_persons = new StreamWriter(this.FPath + "persons.htm", false, Encoding.GetEncoding(1251));
			base.WriteHeader(fs_persons, GKL.LSList[477]);
			fs_persons.WriteLine("<b>" + GKL.LSList[478] + ":</b><ul>");
			TStringList names = new TStringList();
			try
			{
				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
							names.AddObject(TGenEngine.GetNameStr(ind, true, false), ind);
						}
						i++;
					}
					while (i != num);
				}
				names.Sort();

				int num2 = names.Count - 1;
				i = 0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TGEDCOMIndividualRecord ind = names.GetObject(i) as TGEDCOMIndividualRecord;
						fs_persons.WriteLine(string.Concat(new string[]
						{
							"<li><a name=\"", 
							ind.XRef, 
							"\">", 
							names[i], 
							"</a><p>"
						}));
						this.GenTree(fs_persons, ind);
						fs_persons.WriteLine("</p></li>");
						i++;
					}
					while (i != num2);
				}
			}
			finally
			{
				names.Free();
				fs_persons.WriteLine("</ul><hr>");
				base.WriteFooter(fs_persons);
				TObjectHelper.Free(fs_persons);
			}
		}

		private void WriteTimeLineIndex(StreamWriter aStream, string evName, string tlFileName)
		{
			TStringList years = new TStringList();
			try
			{
				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
							TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(ind, evName);
							int year = -1;
							if (ev == null)
							{
								year = -1;
							}
							else
							{
								ushort j;
								ushort d;
								TGenEngine.GetIndependentDate(ev.Detail.Date.Value, out year, out j, out d);
								if (year == 0)
								{
									year = -1;
								}
							}
							string yst;
							if (year < 0)
							{
								yst = "?";
							}
							else
							{
								yst = year.ToString();
							}
							int k = years.IndexOf(yst);
							if (k < 0)
							{
								k = years.AddObject(yst, new TStringList());
							}
							(years.GetObject(k) as TStringList).AddObject(TGenEngine.GetNameStr(ind, true, false), ind);
						}
						i++;
					}
					while (i != num);
				}

				StreamWriter fs_timeline = new StreamWriter(this.FPath + tlFileName, false, Encoding.GetEncoding(1251));
				try
				{
					base.WriteHeader(fs_timeline, GKL.LSList[477]);
					fs_timeline.WriteLine("<b>Индекс:</b><ul>");
					years.Sort();
					string index_str = "";

					int num2 = years.Count - 1;
					i = 0;
					if (num2 >= i)
					{
						num2++;
						do
						{
							index_str = string.Concat(new string[]
							{
								index_str, 
								"<a href=\"", 
								tlFileName, 
								"#", 
								years[i], 
								"\">", 
								years[i], 
								"</a>"
							});
							if (i < years.Count - 1)
							{
								index_str += " | ";
							}
							fs_timeline.WriteLine(string.Concat(new string[]
							{
								"<li><a name=\"", 
								years[i], 
								"\">", 
								years[i], 
								"</a><ul>"
							}));
							TStringList fams = years.GetObject(i) as TStringList;
							fams.Sort();

							int num3 = fams.Count - 1;
							int k = 0;
							if (num3 >= k)
							{
								num3++;
								do
								{
									TGEDCOMIndividualRecord ind = fams.GetObject(k) as TGEDCOMIndividualRecord;
									fs_timeline.WriteLine(string.Concat(new string[]
									{
										"<li><a href=\"persons.htm#", 
										ind.XRef, 
										"\">", 
										fams[k], 
										"</a></li>"
									}));
									k++;
								}
								while (k != num3);
							}
							fs_timeline.WriteLine("</ul></li>");
							i++;
						}
						while (i != num2);
					}
					aStream.WriteLine("<center>" + index_str + "</center>");
					fs_timeline.WriteLine("</ul><hr>");
					base.WriteFooter(fs_timeline);
				}
				finally
				{
					TObjectHelper.Free(fs_timeline);
				}
			}
			finally
			{
				int num4 = years.Count - 1;
				int i = 0;
				if (num4 >= i)
				{
					num4++;
					do
					{
						TObjectHelper.Free(years.GetObject(i));
						i++;
					}
					while (i != num4);
				}
				years.Free();
			}
		}

		private void WriteNameIndex(StreamWriter aStream)
		{
			TStringList[] lat = new TStringList[26];
			TStringList[] rus = new TStringList[32];

			char c;
			for (c = 'A'; c <= 'Z'; c++) lat[(int)c - 65] = null;
			for (c = 'А'; c <= 'Я'; c++) rus[(int)c - 1040] = null;
			TStringList unk = null;

			StreamWriter fs_names = new StreamWriter(this.FPath + "index_names.htm", false, Encoding.GetEncoding(1251));
			base.WriteHeader(fs_names, GKL.LSList[477]);
			fs_names.WriteLine("<ul>");
			try
			{
				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
							string fam;
							string nam;
							string pat;
							TGenEngine.GetNameParts(ind, out fam, out nam, out pat);
							if (nam == "")
							{
								if (unk == null)
								{
									unk = new TStringList();
									unk.Sorted = true;
								}
								unk.AddObject(TGenEngine.GetNameStr(ind, false, false), ind);
							}
							else
							{
								c = nam[0];
								if (IsLatSym(c))
								{
									if (lat[(int)c - 65] == null)
									{
										lat[(int)c - 65] = new TStringList();
										lat[(int)c - 65].Sorted = true;
									}
									lat[(int)c - 65].AddObject(TGenEngine.GetNameStr(ind, false, false), ind);
								}
								else
								{
									if (IsRusSym(c))
									{
										if (rus[(int)c - 1040] == null)
										{
											rus[(int)c - 1040] = new TStringList();
											rus[(int)c - 1040].Sorted = true;
										}
										rus[(int)c - 1040].AddObject(TGenEngine.GetNameStr(ind, false, false), ind);
									}
									else
									{
										if (unk == null)
										{
											unk = new TStringList();
											unk.Sorted = true;
										}
										unk.AddObject(TGenEngine.GetNameStr(ind, false, false), ind);
									}
								}
							}
						}
						i++;
					}
					while (i != num);
				}
				string index_str = "";

				c = 'A';
				do
				{
					if (lat[(int)c - 65] != null)
					{
						index_str = string.Concat(new string[]
						{
							index_str + "<a href=\"index_names.htm#" + SysUtils.NumUpdate((int)c, 3) + "\">" + c + "</a>&nbsp;"
						});
						fs_names.WriteLine(string.Concat(new string[]
						{
							"<li><a name=\"" + SysUtils.NumUpdate((int)c, 3) + "\"><b>" + c + "</b></a>"
						}));
						this.WriteIndex(fs_names, lat[(int)c - 65]);
						fs_names.WriteLine("</li>");
					}
					else
					{
						index_str = index_str + c + "&nbsp;";
					}
					c += '\u0001';
				}
				while (c != '[');

				c = 'А';
				do
				{
					if (rus[(int)c - 1040] != null)
					{
						index_str = index_str + "<a href=\"index_names.htm#" + SysUtils.NumUpdate((int)c, 3) + "\">" + c + "</a>&nbsp;";

						fs_names.WriteLine("<li><a name=\"" + SysUtils.NumUpdate((int)c, 3) + "\"><b>" + c + "</b></a>");
						this.WriteIndex(fs_names, rus[(int)c - 1040]);
						fs_names.WriteLine("</li>");
					}
					else
					{
						index_str = index_str + c + "&nbsp;";
					}
					c += '\u0001';
				}
				while (c != 'а');

				if (unk != null)
				{
					index_str = index_str + "<a href=\"index_names.htm#" + SysUtils.NumUpdate(63, 3) + "\">?</a>&nbsp;";
					fs_names.WriteLine(string.Concat(new string[]
					{
						"<li><a name=\"", SysUtils.NumUpdate(63, 3), "\"><b>", "?", "</b></a>"
					}));
					this.WriteIndex(fs_names, unk);
					fs_names.WriteLine("</li>");
				}

				aStream.WriteLine("<li>" + index_str + "</li>");
			}
			finally
			{
				fs_names.WriteLine("</ul>");
				base.WriteFooter(fs_names);
				TObjectHelper.Free(fs_names);

				for (c = 'A'; c <= 'Z'; c++) {
					TStringList lst = lat[(int)c - 65];
					if (lst != null) lst.Free();
				}

				for (c = 'А'; c <= 'Я'; c++) {
					TStringList lst = rus[(int)c - 1040];
					if (lst != null) lst.Free();
				}

				unk.Free();
			}
		}

		private void WriteFamilyIndex(StreamWriter aStream)
		{
			TStringList[] lat = new TStringList[26];
			TStringList[] rus = new TStringList[32];

			char c;
			for (c = 'A'; c <= 'Z'; c++) lat[(int)c - 65] = null;
			for (c = 'А'; c <= 'Я'; c++) rus[(int)c - 1040] = null;
			TStringList unk = null;

			StreamWriter fs_surnames = new StreamWriter(this.FPath + "index_surnames.htm", false, Encoding.GetEncoding(1251));
			base.WriteHeader(fs_surnames, GKL.LSList[477]);
			fs_surnames.WriteLine("<ul>");
			try
			{
				int num = this.FTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
							string fam;
							string nam;
							string pat;
							TGenEngine.GetNameParts(ind, out fam, out nam, out pat);
							if (fam == "")
							{
								if (unk == null)
								{
									unk = new TStringList();
									unk.Sorted = true;
								}
								unk.AddObject(TGenEngine.GetNameStr(ind, true, false), ind);
							}
							else
							{
								c = fam[0];
								if (IsLatSym(c))
								{
									if (lat[(int)c - 65] == null)
									{
										lat[(int)c - 65] = new TStringList();
										lat[(int)c - 65].Sorted = true;
									}
									lat[(int)c - 65].AddObject(TGenEngine.GetNameStr(ind, true, false), ind);
								}
								else
								{
									if (IsRusSym(c))
									{
										if (rus[(int)c - 1040] == null)
										{
											rus[(int)c - 1040] = new TStringList();
											rus[(int)c - 1040].Sorted = true;
										}
										rus[(int)c - 1040].AddObject(TGenEngine.GetNameStr(ind, true, false), ind);
									}
									else
									{
										if (unk == null)
										{
											unk = new TStringList();
											unk.Sorted = true;
										}
										unk.AddObject(TGenEngine.GetNameStr(ind, true, false), ind);
									}
								}
							}
						}
						i++;
					}
					while (i != num);
				}

				string index_str = "";
				c = 'A';
				do
				{
					if (lat[(int)c - 65] != null)
					{
						index_str = string.Concat(new string[]
						{
							index_str + "<a href=\"index_surnames.htm#" + SysUtils.NumUpdate((int)c, 3) + "\">" + c + "</a>&nbsp;"
						});
						this.WriteSurnames(fs_surnames, c, lat[(int)c - 65]);
					}
					else
					{
						index_str = index_str + c + "&nbsp;";
					}
					c += '\u0001';
				}
				while (c != '[');
				c = 'А';
				do
				{
					if (rus[(int)c - 1040] != null)
					{
						index_str = string.Concat(new string[]
						{
							index_str + "<a href=\"index_surnames.htm#" + SysUtils.NumUpdate((int)c, 3) + "\">" + c + "</a>&nbsp;"
						});
						this.WriteSurnames(fs_surnames, c, rus[(int)c - 1040]);
					}
					else
					{
						index_str = index_str + c + "&nbsp;";
					}
					c += '\u0001';
				}
				while (c != 'а');

				if (unk != null)
				{
					index_str = index_str + "<a href=\"index_surnames.htm#" + SysUtils.NumUpdate(63, 3) + "\">?</a>" + "&nbsp;";
					this.WriteSurnames(fs_surnames, '?', unk);
				}
				aStream.WriteLine("<li>" + index_str + "</li>");
			}
			finally
			{
				fs_surnames.WriteLine("</ul>");
				base.WriteFooter(fs_surnames);
				TObjectHelper.Free(fs_surnames);

				for (c = 'A'; c <= 'Z'; c++) {
					TStringList lst = lat[(int)c - 65];
					if (lst != null) lst.Free();
				}

				for (c = 'А'; c <= 'Я'; c++) {
					TStringList lst = rus[(int)c - 1040];
					if (lst != null) lst.Free();
				}

				unk.Free();
			}
		}

		private void WriteIndex(StreamWriter aStream, TStringList aIndex)
		{
			aStream.WriteLine("<ul>");
			for (int i = 0; i <= aIndex.Count - 1; i++) {
				TGEDCOMIndividualRecord i_rec = aIndex.GetObject(i) as TGEDCOMIndividualRecord;
				aStream.WriteLine(string.Concat(new string[]
				{ "<li><a href=\"persons.htm#", i_rec.XRef, "\">", aIndex[i], "</a></li>" }));
			}
			aStream.WriteLine("</ul>");
		}

		private void WriteSurnames(StreamWriter aStream, char aSym, TStringList aNames)
		{
			aStream.WriteLine("<li><a name=\"" + SysUtils.NumUpdate((int)aSym, 3) + "\"><b>" + aSym + "</b></a><ul>");

			TStringList index = new TStringList();
			try
			{
				int num = aNames.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMIndividualRecord i_rec = aNames.GetObject(i) as TGEDCOMIndividualRecord;
					string f, j, p;
					TGenEngine.GetNameParts(i_rec, out f, out j, out p);
					f = TGenEngine.PrepareRusFamily(f, i_rec.Sex == TGEDCOMSex.svFemale);
					int idx = index.IndexOf(f);
					if (idx < 0)
					{
						idx = index.AddObject(f, new TStringList());
					}
					(index.GetObject(idx) as TStringList).AddObject(aNames[i], i_rec);
				}
				this.FSurnamesCount += index.Count;

				int num2 = index.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TStringList nList = index.GetObject(i) as TStringList;
					aStream.WriteLine(string.Concat(new string[]
					{
						"<li><u>", 
						index[i], 
						"</u> (", 
						nList.Count.ToString(), 
						")"
					}));
					this.WriteIndex(aStream, nList);
					aStream.WriteLine("</li>");
				}
			}
			finally
			{
				int num3 = index.Count - 1;
				for (int i = 0; i <= num3; i++)
				{
					TObjectHelper.Free(index.GetObject(i));
				}
				index.Free();
			}
			aStream.WriteLine("</ul></li>");
		}

		public override void Generate()
		{
			StreamWriter main_index = new StreamWriter(this.FPath + "index.htm", false, Encoding.GetEncoding(1251));
			try
			{
				base.WriteHeader(main_index, GKL.LSList[477]);

				this.FSurnamesCount = 0;
				main_index.WriteLine("<b>" + GKL.LSList[479] + ":</b><ul>");
				this.WriteFamilyIndex(main_index);
				main_index.WriteLine("</ul><hr>");

				main_index.WriteLine("<b>" + GKL.LSList[480] + ":</b><ul>");
				this.WriteNameIndex(main_index);
				main_index.WriteLine("</ul><hr>");

				main_index.WriteLine("<b>" + GKL.LSList[481] + ":</b><ul>");
				this.WriteTimeLineIndex(main_index, "BIRT", "index_birth.htm");
				main_index.WriteLine("</ul><hr>");

				main_index.WriteLine("<b>" + GKL.LSList[482] + ":</b><ul>");
				this.WriteTimeLineIndex(main_index, "DEAT", "index_death.htm");
				main_index.WriteLine("</ul><hr>");

				this.WritePersons();

				TGenEngine.TCommonStats stats;
				this.FEngine.GetCommonStats(out stats);
				main_index.WriteLine("<b>" + GKL.LSList[483] + ":</b><ul>");
				main_index.WriteLine("<li>Персон: " + stats.persons.ToString() + "</li>");
				main_index.WriteLine("<li>Фамилий: " + this.FSurnamesCount.ToString() + "</li>");
				main_index.WriteLine("</ul><hr>");

				base.WriteFooter(main_index);
			}
			finally
			{
				TObjectHelper.Free(main_index);
			}
			SysUtils.LoadExtFile(this.FPath + "index.htm");
		}

		public TWebExporter(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}
	}
}
