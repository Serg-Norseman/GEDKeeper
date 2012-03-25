using System;
using System.IO;
using System.Text;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public class WebExporter : HTMLExporter
	{
		private int FSurnamesCount;

		private bool IsLatSym(char c)
		{
			return (c >= 'A' && c <= 'Z');
		}

		private bool IsRusSym(char c)
		{
			return (c >= 'А' && c <= 'Я');
		}

		private void WritePersons()
		{
			StreamWriter fs_persons = new StreamWriter(this.FPath + "persons.htm", false, Encoding.GetEncoding(1251));
			base.WriteHTMLHeader(fs_persons, LangMan.LSList[477]);
			fs_persons.WriteLine("<b>" + LangMan.LSList[478] + ":</b><ul>");
			StringList names = new StringList();
			try
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
						names.AddObject(ind.aux_GetNameStr(true, false), ind);
					}
				}
				names.Sort();

				int num2 = names.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TGEDCOMIndividualRecord ind = names.GetObject(i) as TGEDCOMIndividualRecord;
					fs_persons.WriteLine("<li><a name=\"" + ind.XRef + "\">" + names[i] + "</a><p>");

					WebTree wt = new WebTree();
					wt.GenTree(fs_persons, ind);

					fs_persons.WriteLine("</p></li>");
				}
			}
			finally
			{
				names.Free();
				fs_persons.WriteLine("</ul><hr>");
				base.WriteHTMLFooter(fs_persons);
				SysUtils.Free(fs_persons);
			}
		}

		private void WriteSpecialIndex(string filename, string title, StringList specIndex, bool id_links)
		{
			try
			{
				using (StreamWriter stm = new StreamWriter(this.FPath + filename, false, Encoding.GetEncoding(1251)))
				{
					base.WriteHTMLHeader(stm, LangMan.LSList[477]);
					stm.WriteLine("<b>"+title+":</b><ul>");

					specIndex.Sort();
					int num2 = specIndex.Count - 1;
					for (int i = 0; i <= num2; i++)
					{
						string id = specIndex[i];
						if (id_links) id = "<a name=\"" + id + "\">" + id + "</a>";
						stm.WriteLine("<li>" + id + "<ul>");

						StringList personsIndex = specIndex.GetObject(i) as StringList;

						personsIndex.Sort();
						int num3 = personsIndex.Count - 1;
						for (int k = 0; k <= num3; k++)
						{
							TGEDCOMIndividualRecord ind = personsIndex.GetObject(k) as TGEDCOMIndividualRecord;
							stm.WriteLine("<li><a href=\"persons.htm#" + ind.XRef + "\">" + personsIndex[k] + "</a></li>");
						}

						stm.WriteLine("</ul></li><br>");
					}
					stm.WriteLine("</ul>");
					base.WriteHTMLFooter(stm);
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("WebExporter.WriteSpecialIndex(): " + ex.Message);
			}
		}

		private void PrepareSpecIndex(StringList index, string val, TGEDCOMIndividualRecord iRec)
		{
			StringList persons;

			int idx = index.IndexOf(val);
			if (idx < 0) {
				persons = new StringList();
				index.AddObject(val, persons);
			} else {
				persons = (StringList)index.GetObject(idx);
			}

			if (persons.IndexOfObject(iRec) < 0) {
				persons.AddObject(iRec.aux_GetNameStr(true, false), iRec);
			}
		}

		private void PrepareEventYear(StringList index, TGEDCOMCustomEvent evt, TGEDCOMIndividualRecord iRec)
		{
			int year = -1;
			if (evt == null)
			{
				year = -1;
			}
			else
			{
				ushort m, d;
				evt.Detail.Date.aux_GetIndependentDate(out year, out m, out d);
				if (year == 0) year = -1;
			}

			string yst = ((year < 0) ? "?" : year.ToString());
			PrepareSpecIndex(index, yst, iRec);
		}

		private void WriteSpecials(StreamWriter mainIndex)
		{
			string dc_file = "death_causes.htm";
			string occ_file = "occupations.htm";
			string places_file = "places.htm";
			string sources_file = "sources.htm";

			string birth_file = "index_birth.htm";
			string death_file = "index_death.htm";

			StringList birthes = new StringList();
			StringList deathes = new StringList();

			StringList dcauses = new StringList();
			StringList occupations = new StringList();
			StringList places = new StringList();
			StringList sources = new StringList();
			try
			{
				string st;
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;

						int src_num = iRec.SourceCitations.Count - 1;
						for (int k = 0; k <= src_num; k++)
						{
							TGEDCOMSourceRecord src = iRec.SourceCitations[k].Value as TGEDCOMSourceRecord;
							if (src != null)
							{
								st = src.FiledByEntry;
								if (string.IsNullOrEmpty(st)) st = src.Title.Text;
								PrepareSpecIndex(sources, st, iRec);
							}
						}

						int ev_num = iRec.IndividualEvents.Count - 1;
						for (int k = 0; k <= ev_num; k++)
						{
							TGEDCOMCustomEvent ev = iRec.IndividualEvents[k];
							if (ev != null)
							{
								int src_num2 = ev.Detail.SourceCitations.Count - 1;
								for (int m = 0; m <= src_num2; m++)
								{
									TGEDCOMSourceRecord src = ev.Detail.SourceCitations[m].Value as TGEDCOMSourceRecord;
									if (src != null)
									{
										st = src.FiledByEntry;
										if (string.IsNullOrEmpty(st)) st = src.Title.Text;
										PrepareSpecIndex(sources, st, iRec);
									}
								}

								// Анализ по местам
								st = ev.Detail.Place.StringValue;
								if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(places, st, iRec);

								// Анализ по рождениям
								if (ev.Name == "BIRT")
								{
									PrepareEventYear(birthes, ev, iRec);
								}

								// Анализ по причинам смерти
								if (ev.Name == "DEAT")
								{
									PrepareEventYear(deathes, ev, iRec);

									st = ev.Detail.Cause;
									if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(dcauses, st, iRec);
								}

								// Анализ по занятиям
								if (ev.Name == "OCCU")
								{
									st = ev.StringValue;
									if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(occupations, st, iRec);
								}
							}
						}
					}
				}


				// Запись индекса годов рождения
				WriteSpecialIndex(birth_file, LangMan.LSList[481], birthes, true);
				string index_str = "";
				int num2 = birthes.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					index_str = index_str + "<a href=\"" + birth_file + "#" + birthes[i] + "\">" + birthes[i] + "</a>";
					if (i < birthes.Count - 1) index_str += " | ";
				}
				mainIndex.WriteLine("<b>" + LangMan.LSList[481] + ":</b><ul><center>" + index_str + "</center></ul><hr>");

				// Запись индекса годов смерти
				WriteSpecialIndex(death_file, LangMan.LSList[482], deathes, true);
				index_str = "";
				num2 = deathes.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					index_str = index_str + "<a href=\"" + death_file + "#" + deathes[i] + "\">" + deathes[i] + "</a>";
					if (i < birthes.Count - 1) index_str += " | ";
				}
				mainIndex.WriteLine("<b>" + LangMan.LSList[482] + ":</b><ul><center>" + index_str + "</center></ul><hr>");


				// Запись индекса мест
				WriteSpecialIndex(places_file, "Места", places, false);
				mainIndex.WriteLine("<p><a href=\""+places_file+"\">Места</a></p><hr>");

				// Запись индекса причин смерти
				WriteSpecialIndex(dc_file, "Причины смерти", dcauses, false);
				mainIndex.WriteLine("<p><a href=\""+dc_file+"\">Причины смерти</a></p><hr>");

				// Запись индекса занятий
				WriteSpecialIndex(occ_file, "Занятия", occupations, false);
				mainIndex.WriteLine("<p><a href=\""+occ_file+"\">Занятия</a></p><hr>");

				// Запись индекса источников
				WriteSpecialIndex(sources_file, "Источники", sources, false);
				mainIndex.WriteLine("<p><a href=\""+sources_file+"\">Источники</a></p><hr>");
			}
			finally
			{
				int num4 = birthes.Count - 1;
				for (int i = 0; i <= num4; i++) SysUtils.Free(birthes.GetObject(i));
				birthes.Free();

				num4 = deathes.Count - 1;
				for (int i = 0; i <= num4; i++) SysUtils.Free(deathes.GetObject(i));
				deathes.Free();

				num4 = dcauses.Count - 1;
				for (int i = 0; i <= num4; i++) SysUtils.Free(dcauses.GetObject(i));
				dcauses.Free();

				num4 = occupations.Count - 1;
				for (int i = 0; i <= num4; i++) SysUtils.Free(occupations.GetObject(i));
				occupations.Free();

				num4 = places.Count - 1;
				for (int i = 0; i <= num4; i++) SysUtils.Free(places.GetObject(i));
				places.Free();

				num4 = sources.Count - 1;
				for (int i = 0; i <= num4; i++) SysUtils.Free(sources.GetObject(i));
				sources.Free();
			}
		}

		private void WriteNameIndex(StreamWriter aStream)
		{
			StringList[] lat = new StringList[26];
			StringList[] rus = new StringList[32];

			char c;
			for (c = 'A'; c <= 'Z'; c++) lat[(int)c - 65] = null;
			for (c = 'А'; c <= 'Я'; c++) rus[(int)c - 1040] = null;
			StringList unk = null;

			StreamWriter fs_names = new StreamWriter(this.FPath + "index_names.htm", false, Encoding.GetEncoding(1251));
			base.WriteHTMLHeader(fs_names, LangMan.LSList[477]);
			fs_names.WriteLine("<ul>");
			try
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
						string fam, nam, pat;
						ind.aux_GetNameParts(out fam, out nam, out pat);
						if (nam == "")
						{
							if (unk == null)
							{
								unk = new StringList();
								unk.Sorted = true;
							}
							unk.AddObject(ind.aux_GetNameStr(false, false), ind);
						}
						else
						{
							c = nam[0];
							if (IsLatSym(c))
							{
								if (lat[(int)c - 65] == null)
								{
									lat[(int)c - 65] = new StringList();
									lat[(int)c - 65].Sorted = true;
								}
								lat[(int)c - 65].AddObject(ind.aux_GetNameStr(false, false), ind);
							}
							else
							{
								if (IsRusSym(c))
								{
									if (rus[(int)c - 1040] == null)
									{
										rus[(int)c - 1040] = new StringList();
										rus[(int)c - 1040].Sorted = true;
									}
									rus[(int)c - 1040].AddObject(ind.aux_GetNameStr(false, false), ind);
								}
								else
								{
									if (unk == null)
									{
										unk = new StringList();
										unk.Sorted = true;
									}
									unk.AddObject(ind.aux_GetNameStr(false, false), ind);
								}
							}
						}
					}
				}
				string index_str = "";

				for (c = 'A'; c <= 'Z'; c++)
				{
					if (lat[(int)c - 65] != null)
					{
						index_str = index_str + "<a href=\"index_names.htm#" + TGenEngine.NumUpdate((int)c, 3) + "\">" + c + "</a>&nbsp;";
						fs_names.WriteLine("<li><a name=\"" + TGenEngine.NumUpdate((int)c, 3) + "\"><b>" + c + "</b></a>");
						this.WriteIndex(fs_names, lat[(int)c - 65]);
						fs_names.WriteLine("</li>");
					}
					else
					{
						index_str = index_str + c + "&nbsp;";
					}
				}

				for (c = 'А'; c <= 'Я'; c++)
				{
					if (rus[(int)c - 1040] != null)
					{
						index_str = index_str + "<a href=\"index_names.htm#" + TGenEngine.NumUpdate((int)c, 3) + "\">" + c + "</a>&nbsp;";
						fs_names.WriteLine("<li><a name=\"" + TGenEngine.NumUpdate((int)c, 3) + "\"><b>" + c + "</b></a>");
						this.WriteIndex(fs_names, rus[(int)c - 1040]);
						fs_names.WriteLine("</li>");
					}
					else
					{
						index_str = index_str + c + "&nbsp;";
					}
				}

				if (unk != null)
				{
					index_str = index_str + "<a href=\"index_names.htm#" + TGenEngine.NumUpdate(63, 3) + "\">?</a>&nbsp;";
					fs_names.WriteLine("<li><a name=\"" + TGenEngine.NumUpdate(63, 3) + "\"><b>", "?", "</b></a>");
					this.WriteIndex(fs_names, unk);
					fs_names.WriteLine("</li>");
				}

				aStream.WriteLine("<li>" + index_str + "</li>");
			}
			finally
			{
				fs_names.WriteLine("</ul>");
				base.WriteHTMLFooter(fs_names);
				SysUtils.Free(fs_names);

				for (c = 'A'; c <= 'Z'; c++) {
					StringList lst = lat[(int)c - 65];
					if (lst != null) lst.Free();
				}

				for (c = 'А'; c <= 'Я'; c++) {
					StringList lst = rus[(int)c - 1040];
					if (lst != null) lst.Free();
				}

				unk.Free();
			}
		}

		private void WriteFamilyIndex(StreamWriter aStream)
		{
			StringList[] lat = new StringList[26];
			StringList[] rus = new StringList[32];

			char c;
			for (c = 'A'; c <= 'Z'; c++) lat[(int)c - 65] = null;
			for (c = 'А'; c <= 'Я'; c++) rus[(int)c - 1040] = null;
			StringList unk = null;

			StreamWriter fs_surnames = new StreamWriter(this.FPath + "index_surnames.htm", false, Encoding.GetEncoding(1251));
			base.WriteHTMLHeader(fs_surnames, LangMan.LSList[477]);
			fs_surnames.WriteLine("<ul>");
			try
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
						string fam, nam, pat;
						ind.aux_GetNameParts(out fam, out nam, out pat);
						if (fam == "")
						{
							if (unk == null)
							{
								unk = new StringList();
								unk.Sorted = true;
							}
							unk.AddObject(ind.aux_GetNameStr(true, false), ind);
						}
						else
						{
							c = fam[0];
							if (IsLatSym(c))
							{
								if (lat[(int)c - 65] == null)
								{
									lat[(int)c - 65] = new StringList();
									lat[(int)c - 65].Sorted = true;
								}
								lat[(int)c - 65].AddObject(ind.aux_GetNameStr(true, false), ind);
							}
							else
							{
								if (IsRusSym(c))
								{
									if (rus[(int)c - 1040] == null)
									{
										rus[(int)c - 1040] = new StringList();
										rus[(int)c - 1040].Sorted = true;
									}
									rus[(int)c - 1040].AddObject(ind.aux_GetNameStr(true, false), ind);
								}
								else
								{
									if (unk == null)
									{
										unk = new StringList();
										unk.Sorted = true;
									}
									unk.AddObject(ind.aux_GetNameStr(true, false), ind);
								}
							}
						}
					}
				}

				string index_str = "";
				for (c = 'A'; c <= 'Z'; c++)
				{
					if (lat[(int)c - 65] != null)
					{
						index_str = index_str + "<a href=\"index_surnames.htm#" + TGenEngine.NumUpdate((int)c, 3) + "\">" + c + "</a>&nbsp;";
						this.WriteSurnames(fs_surnames, c, lat[(int)c - 65]);
					}
					else
					{
						index_str = index_str + c + "&nbsp;";
					}
				}

				for (c = 'А'; c <= 'Я'; c++)
				{
					if (rus[(int)c - 1040] != null)
					{
						index_str = index_str + "<a href=\"index_surnames.htm#" + TGenEngine.NumUpdate((int)c, 3) + "\">" + c + "</a>&nbsp;";
						this.WriteSurnames(fs_surnames, c, rus[(int)c - 1040]);
					}
					else
					{
						index_str = index_str + c + "&nbsp;";
					}
				}

				if (unk != null)
				{
					index_str = index_str + "<a href=\"index_surnames.htm#" + TGenEngine.NumUpdate(63, 3) + "\">?</a>" + "&nbsp;";
					this.WriteSurnames(fs_surnames, '?', unk);
				}
				aStream.WriteLine("<li>" + index_str + "</li>");
			}
			finally
			{
				fs_surnames.WriteLine("</ul>");
				base.WriteHTMLFooter(fs_surnames);
				SysUtils.Free(fs_surnames);

				for (c = 'A'; c <= 'Z'; c++) {
					StringList lst = lat[(int)c - 65];
					if (lst != null) lst.Free();
				}

				for (c = 'А'; c <= 'Я'; c++) {
					StringList lst = rus[(int)c - 1040];
					if (lst != null) lst.Free();
				}

				unk.Free();
			}
		}

		private void WriteIndex(StreamWriter aStream, StringList aIndex)
		{
			aStream.WriteLine("<ul>");
			for (int i = 0; i <= aIndex.Count - 1; i++) {
				TGEDCOMIndividualRecord i_rec = aIndex.GetObject(i) as TGEDCOMIndividualRecord;
				aStream.WriteLine("<li><a href=\"persons.htm#" + i_rec.XRef + "\">" + aIndex[i] + "</a></li>");
			}
			aStream.WriteLine("</ul>");
		}

		private void WriteSurnames(StreamWriter aStream, char aSym, StringList aNames)
		{
			aStream.WriteLine("<li><a name=\"" + TGenEngine.NumUpdate((int)aSym, 3) + "\"><b>" + aSym + "</b></a><ul>");

			StringList index = new StringList();
			try
			{
				int num = aNames.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMIndividualRecord i_rec = aNames.GetObject(i) as TGEDCOMIndividualRecord;
					string f, j, p;
					i_rec.aux_GetNameParts(out f, out j, out p);
					f = TGenEngine.PrepareRusFamily(f, i_rec.Sex == TGEDCOMSex.svFemale);
					int idx = index.IndexOf(f);
					if (idx < 0)
					{
						idx = index.AddObject(f, new StringList());
					}
					(index.GetObject(idx) as StringList).AddObject(aNames[i], i_rec);
				}
				this.FSurnamesCount += index.Count;

				int num2 = index.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					StringList nList = index.GetObject(i) as StringList;
					aStream.WriteLine("<li><u>" + index[i] + "</u> (" + nList.Count.ToString() + ")");
					this.WriteIndex(aStream, nList);
					aStream.WriteLine("</li>");
				}
			}
			finally
			{
				int num3 = index.Count - 1;
				for (int i = 0; i <= num3; i++) SysUtils.Free(index.GetObject(i));
				index.Free();
			}
			aStream.WriteLine("</ul></li>");
		}

		public override void Generate()
		{
			using (StreamWriter contents = new StreamWriter(this.FPath + "index.htm", false, Encoding.GetEncoding(1251)))
			{
				base.WriteHTMLHeader(contents, LangMan.LSList[477]);

				this.FSurnamesCount = 0;
				contents.WriteLine("<b>" + LangMan.LSList[479] + ":</b><ul>");
				this.WriteFamilyIndex(contents);
				contents.WriteLine("</ul><hr>");

				contents.WriteLine("<b>" + LangMan.LSList[480] + ":</b><ul>");
				this.WriteNameIndex(contents);
				contents.WriteLine("</ul><hr>");

				this.WritePersons();
				this.WriteSpecials(contents);

				TGenEngine.TCommonStats stats;
				this.FEngine.GetCommonStats(out stats);
				contents.WriteLine("<b>" + LangMan.LSList[483] + ":</b><ul>");
				contents.WriteLine("<li>Персон: " + stats.persons.ToString() + "</li>");
				contents.WriteLine("<li>Фамилий: " + this.FSurnamesCount.ToString() + "</li>");
				contents.WriteLine("</ul><hr>");

				base.WriteHTMLFooter(contents);
			}
			TGenEngine.LoadExtFile(this.FPath + "index.htm");
		}

		public WebExporter(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}
	}
}
