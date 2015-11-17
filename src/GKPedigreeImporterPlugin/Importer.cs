using System;
using System.IO;
using System.Text;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Interfaces;
using MSOExcel = Microsoft.Office.Interop.Excel;
using MSOWord = Microsoft.Office.Interop.Word;

namespace GKPedigreeImporterPlugin
{
    [Serializable]
    public class ImporterException : Exception
    {
        public ImporterException()
        {
        }

        public ImporterException(string message) : base(message)
        {
        }
    }

    /// <summary>
    /// Localization: dirty
    /// </summary>
    public class Importer : BaseObject
	{
    	private readonly IBaseWindow fBase;
		private readonly GEDCOMTree fTree;
		private readonly System.Windows.Forms.ListBox.ObjectCollection fLog;
		private StringList fPersonsList;
		private ILangMan fLangMan;

        public Importer(IBaseWindow aBase, ILangMan langMan, System.Windows.Forms.ListBox.ObjectCollection aLog)
        {
        	this.fBase = aBase;
            this.fTree = aBase.Tree;
            this.fLog = aLog;
            this.fLangMan = langMan;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fPersonsList != null) fPersonsList.Dispose();
            }
            base.Dispose(disposing);
        }

        private void AddChild(GEDCOMIndividualRecord parent, GEDCOMIndividualRecord child, int mar_id)
		{
			if (mar_id < 0)
			{
				mar_id = 1;
			}

			if (mar_id > 1)
			{
				// ???
			}

			GEDCOMSex sex = parent.Sex;
			if (sex == GEDCOMSex.svNone || sex == GEDCOMSex.svUndetermined)
			{
				parent.Sex = GEDCOMSex.svMale;
			}

			while (parent.SpouseToFamilyLinks.Count < mar_id)
			{
				this.AddFamily(parent);
			}
			mar_id--;

			GEDCOMFamilyRecord family = parent.SpouseToFamilyLinks[mar_id].Family;
			GEDCOMPointer ptr = new GEDCOMPointer(this.fTree, family, "", "");
			ptr.SetNamedValue("CHIL", child);
			family.Childrens.Add(ptr);

			GEDCOMChildToFamilyLink chLink = new GEDCOMChildToFamilyLink(this.fTree, child, "", "");
			chLink.Family = family;
			child.ChildToFamilyLinks.Add(chLink);
		}

		private GEDCOMFamilyRecord AddFamily(GEDCOMIndividualRecord parent)
		{
			GEDCOMFamilyRecord result = this.fTree.CreateFamily();
			result.AddSpouse(parent);
			return result;
		}

		private static string CheckDot(string aStr)
		{
			if (!string.IsNullOrEmpty(aStr))
			{
				if (aStr[aStr.Length - 1] == '.')
				{
					aStr = aStr.Substring(0, aStr.Length - 1);
				}
			}
			return aStr.Trim();
		}

		private void DefinePersonName(string aStr, string p_id, ref string f_name, ref string f_pat, ref string f_fam, ref string bd, ref string dd)
		{
			f_name = "";
			f_pat = "";
			f_fam = "";
			bd = "";
			dd = "";
			string tmp = aStr;
			int num = ((p_id != null) ? p_id.Length : 0) + 2;
			tmp = tmp.Remove(0, num);
			tmp = CheckDot(tmp);

			int b_pos = tmp.IndexOf("*");
			int d_pos = tmp.IndexOf("+");
			if (d_pos >= 0 && d_pos > b_pos)
			{
				dd = tmp.Substring(d_pos + 1, tmp.Length - d_pos - 1);
				int num2 = ((dd != null) ? dd.Length : 0) + 1;
				tmp = tmp.Remove(d_pos, num2);
				tmp = tmp.Trim();
			}

			if (b_pos >= 0)
			{
				bd = tmp.Substring(b_pos + 1, tmp.Length - b_pos - 1);
				int num3 = ((bd != null) ? bd.Length : 0) + 1;
				tmp = tmp.Remove(b_pos, num3);
				tmp = tmp.Trim();
			}

			string[] tokens = tmp.Trim().Split(' ');
			if (tokens.Length > 0) f_name = CheckDot(tokens[0]);
			if (tokens.Length > 1) f_pat = CheckDot(tokens[1]);
			if (tokens.Length > 2) f_fam = CheckDot(tokens[2]);
		}

		private static string DeleteBlanks(string S)
		{
			string result = S;
			if (result != null)
			{
				int I = 1;
				while (I <= result.Length)
				{
					if (result[I - 1] == ' ')
					{
						result = result.Remove(I - 1, 1);
					}
					else
					{
						I++;
					}
				}
			}
			return result;
		}

		private static string ExtractNumComment(string S, ref string Comment, bool NoException)
		{
			string result = S;

			if (!string.IsNullOrEmpty(result))
			{
				Comment = "";
				result = result.Remove(0, 1);
				int I = 0;
				while (I < result.Length && result[I] != ')')
				{
					I++;
				}
				if (I > 0)
				{
					Comment = result.Substring(0, I);
					result = result.Remove(0, I);
				}
				result = result.Remove(0, 1);
			}

			return result;
		}

		private const string PersonIdChars = "0123456789- ()/?";

		private static bool IsPersonLine(string aStr, ref string p_id)
		{
			p_id = "";

			int i = 1;
			while (i <= aStr.Length)
			{
				char c = aStr[i - 1];
				if (PersonIdChars.IndexOf(c) < 0) break;

				p_id += aStr[i - 1];
				i++;

				if (aStr[i - 1] == '(')
				{
					while (i <= aStr.Length && aStr[i - 1] != ')')
					{
						p_id += aStr[i - 1];
						i++;
					}
				}
			}

			return (p_id != "" && aStr[i - 1] == '.' && aStr[i] == ' ');
		}

		private static bool IsRomeChar(char c)
		{
			return (c == 'I' || c == 'V' || c == 'X' || c == 'L' || c == 'C' || c == 'D' || c == 'M');
		}

		private static bool IsRomeLine(string aStr)
		{
			int i = 1;
			string rs = "";
			while (i <= ((aStr != null) ? aStr.Length : 0) && IsRomeChar(aStr[i - 1]))
			{
				rs += aStr[i - 1];
				i++;
			}
			return (rs != "" && rs == aStr);
		}

		private void SetEvent(GEDCOMIndividualRecord iRec, string evName, string date)
		{
			int[] val = new int[3];
			GEDCOMCustomEvent ev = this.fBase.Context.CreateEventEx(iRec, evName, "", "");
			try
			{
				string prefix = "";
				if (date.IndexOf("п.") == 0)
				{
					prefix = "AFT ";
					date = date.Remove(0, 2).Trim();
				}
				else
				{
					if (date.IndexOf("до") == 0)
					{
						prefix = "BEF ";
						date = date.Remove(0, 3).Trim();
					}
				}

				string tmp = "";
				string[] toks = date.Split('.');
				if (toks.Length > 3)
				{
                    throw new ImporterException("date failed");
				}
				string ym = "";

				for (int i = 0; i < toks.Length; i++)
				{
					tmp = toks[i];

					int x = tmp.IndexOf("/");
					if (x >= 0)
					{
						ym = tmp.Substring(x + 1, tmp.Length - x - 1);
						tmp = tmp.Remove(x, ym.Length + 1);
					}

					val[i] = int.Parse(tmp);
				}

				if (toks.Length != 1)
				{
					if (toks.Length != 2)
					{
						if (toks.Length == 3)
						{
                            tmp = val[0].ToString() + " " + GEDCOMCustomDate.GEDCOMMonthArray[val[1] - 1] + " " + val[2].ToString();
						}
					}
					else
					{
						tmp = GEDCOMCustomDate.GEDCOMMonthArray[val[0] - 1] + " " + val[1].ToString();
					}
				}
				else
				{
					tmp = val[0].ToString();
				}

				tmp = prefix + tmp;
				if (ym != "")
				{
					tmp = tmp + "/" + ym;
				}
				ev.Detail.Date.ParseString(tmp);
			}
			catch (Exception ex)
			{
				this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_DateInvalid) + " \"" + date + "\"");
				this.fBase.Host.LogWrite("Importer.SetEvent(" + date + "): " + ex.Message);
			}
		}

		private GEDCOMIndividualRecord ParsePerson(StringList buf, string aStr, string p_id, ref int self_id)
		{
			self_id = -1;
			int parent_id = -1;
			int mar_id = -1;

			string S = DeleteBlanks(p_id);
			S = GEDCOMUtils.ExtractNumber(S, out self_id, true, -1);

			if (S != "" && S[0] == '-')
			{
				S = S.Remove(0, 1);
				S = GEDCOMUtils.ExtractNumber(S, out parent_id, true, -1);

				if (S != "" && S[0] == '(')
				{
					string com = "";
					S = ExtractNumComment(S, ref com, true);
				}

				if (S != "" && S[0] == '/')
				{
					S = S.Remove(0, 1);
					S = GEDCOMUtils.ExtractNumber(S, out mar_id, true, -1);
				}
			}

			string f_name = "";
			string f_pat = "";
			string f_fam = "";
			string bd = "";
			string dd = "";

			this.DefinePersonName(aStr, p_id, ref f_name, ref f_pat, ref f_fam, ref bd, ref dd);
			GEDCOMIndividualRecord result = this.fBase.Context.CreatePersonEx(f_name, f_pat, f_fam, GEDCOMSex.svNone, false);
			this.fBase.CheckPersonSex(result);
			this.fPersonsList.AddObject(self_id.ToString(), result);

			buf.Add(aStr);

			if (bd != "") this.SetEvent(result, "BIRT", bd);
			if (dd != "") this.SetEvent(result, "DEAT", dd);

			if (parent_id > 0)
			{
				int x = this.fPersonsList.IndexOf(parent_id.ToString());
				if (x >= 0)
				{
					GEDCOMIndividualRecord parent = this.fPersonsList.GetObject(x) as GEDCOMIndividualRecord;
					this.AddChild(parent, result, mar_id);
				}
				else
				{
					this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_AncNotFound) + " \"" + parent_id.ToString() + "\".");
				}
			}
			return result;
		}

		public void Import_PlainText(string fileName)
		{
			try
			{
				StreamReader strd = new StreamReader(fileName, Encoding.GetEncoding(1251));
				StringList content = new StringList();
				try
				{
					while (strd.Peek() != -1) {
						string ns = strd.ReadLine().Trim();
						content.Add(ns);
					}
					content.Add("");

					this.Import_StringList(content);
				}
				finally
				{
					strd.Close();
                    content.Dispose();
				}
			}
			catch (Exception ex)
			{
				this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
				this.fBase.Host.LogWrite("Import_PlainText(): " + ex.Message);
			}
		}

		private const bool DEBUG_EXCEL = true;
		private const bool DEBUG_WORD = true;

		public void Import_Excel(string fileName)
		{
			try
			{
				fLog.Clear();

				MSOExcel.Application excel;
				try
				{
					excel = new MSOExcel.Application();
				}
				catch (Exception ex)
				{
					return;
				}

				excel.Visible = DEBUG_EXCEL;
				excel.DisplayAlerts = false;
				excel.WindowState = MSOExcel.XlWindowState.xlMaximized;
				excel.Workbooks.Open(fileName);
				MSOExcel.Worksheet sheet = excel.Worksheets[1] as MSOExcel.Worksheet;
				sheet.Activate();

				StringList buf = new StringList();
				StringList FPersonsList = new StringList();
				try
				{
					// получаем используемое количество строк и столбцов
					int rows_count = sheet.UsedRange.Rows.Count;
					int cols_count = sheet.UsedRange.Columns.Count;

					int prev_id = 0;
					GEDCOMIndividualRecord i_rec = null;

					for (int row = 1; row <= rows_count; row++)
					{
						string c1 = sheet.Cells[row, 1].ToString().Trim(); // номер позиции
						string c2 = sheet.Cells[row, 2].ToString().Trim(); // номер предка
						string c3 = sheet.Cells[row, 3].ToString().Trim(); // имя, может начинаться с номера брака
						string c4 = sheet.Cells[row, 4].ToString().Trim(); // дата рождения
						string c5 = sheet.Cells[row, 5].ToString().Trim(); // дата смерти
						string c6 = sheet.Cells[row, 6].ToString().Trim(); // место рождения или проживания

						string rome, s, p_id = "";
						int self_id = 0;

						if (c1 == "" && c3 == "") {
							CheckBuf(buf, i_rec);
							i_rec = null;
						} else {
							if (IsRomeLine(c2)) {
								rome = c2;
							} else if (IsRomeLine(c3)) {
								rome = c3;
							} else {
								rome = "";
							}

							if (rome != "") {
								fLog.Add("> " + fLangMan.LS(ILS.LSID_Generation) + " " + rome);
								i_rec = null;
							} else {
								if (c3[1] == '/') {
									s = c1 + c2 + c3 + " " + c4 + " " + c5;
								} else {
									s = c1 + c2 + ". " + c3 + " " + c4 + " " + c5;
								}

								if (c6 != "") {
									s = s + ". " + c6 + ".";
								}

								if (!IsPersonLine(s, ref p_id)) {
									buf.Add(s);
								} else {
									CheckBuf(buf, i_rec);
									i_rec = ParsePerson(buf, s, p_id, ref self_id);

									fLog.Add("> "+fLangMan.LS(ILS.LSID_PersonParsed) + " " + p_id + ".");

									if (self_id - prev_id > 1) {
										fLog.Add(">>>> "+fLangMan.LS(ILS.LSID_ParseError_LineSeq));
									}

									prev_id = self_id;
								}
							}
						}
					}
				}
				finally
				{
					fPersonsList.Dispose();
					buf.Dispose();

					excel.Quit();
					excel = null;
				}
			}
			catch (Exception ex)
			{
				this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
				this.fBase.Host.LogWrite("Import_Excel(): " + ex.Message);
			}
		}

		public void Import_Word(string fileName)
		{
			try
			{
				MSOWord.Application wordApp;
				try
				{
					wordApp = new MSOWord.Application();
				}
				catch
				{
					return;
				}

				StringList content = new StringList();
				try
				{
					wordApp.Visible = DEBUG_WORD;
					wordApp.WindowState = MSOWord.WdWindowState.wdWindowStateMaximize;

					MSOWord.Document doc = wordApp.Documents.Open(fileName);
					for (int i = 0; i < doc.Paragraphs.Count; i++)
					{
						string txt = doc.Paragraphs[i+1].Range.Text.ToString();
						content.Add(txt);
					}					
					content.Add("");

					Import_StringList(content);

					doc.Close();
				}
				finally
				{
					content.Dispose();

					wordApp.Quit();
					wordApp = null;
				}
			}
			catch (Exception ex)
			{
				this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
				this.fBase.Host.LogWrite("Import_Word(): " + ex.Message);
			}
		}

		private void CheckBuf(StringList buf, GEDCOMIndividualRecord iRec)
		{
			if (buf.Text != "")
			{
				if (iRec != null)
				{
					this.CheckSpouses(buf, iRec);
				}
				this.fTree.CreateNoteEx(iRec, buf);
				buf.Clear();
			}
		}

		private void CheckSpouses(StringList buf, GEDCOMIndividualRecord iRec)
		{
			int num2 = buf.Count;
			for (int i = 0; i < num2; i++)
			{
				string s = buf[i];
				if (string.IsNullOrEmpty(s)) continue;

                // skip blanks
                s = SysUtils.TrimChars(s, new char[] { ' ', '.' });

				if (s.Length > 2)
				{
					char c2 = s[0];
					char c3 = s[1];
					if ((c2 == 'М' || c2 == 'Ж') && ((c3 == ' ') || (c3 >= '1' && c3 <= '9')))
					{
						try
						{
							// define sex
							GEDCOMSex sx = (s[0] == 'М') ? GEDCOMSex.svMale : GEDCOMSex.svFemale;
                            s = s.Remove(0, 1);

							// number of spouse
							int num;
							s = GEDCOMUtils.ExtractNumber(s, out num, true, 1);

							// skip blanks
                            s = SysUtils.TrimChars(s, ' ');

							// extract date of marriage
							int p;
							if (s[0] == '(')
							{
								p = 0;
								while (p < s.Length && s[p] != ')')
								{
									p++;
								}
								if (p > 0)
								{
									p++;
									string mar_date = s.Substring(0, p);
									mar_date = mar_date.Trim();
									s = s.Remove(0, p);
								}
							}

							// skip interval before name
                            s = SysUtils.TrimChars(s, new char[] { '–', ' ', '-' });

							// extract name
							p = 0;
							while (p < s.Length && (s[p] != '*' && s[p] != '+' && s[p] != '.')) p++;

							string name = "";
							if (p > 0)
							{
								name = s.Substring(0, p);
								name = name.Trim();
								s = s.Remove(0, p);
							}

							if (name != "")
							{
								GEDCOMFamilyRecord fam = this.AddFamily(iRec);

								string f_name = "";
								string f_pat = "";
								string f_fam = "";
								string[] nm_parts = name.Trim().Split(' ');
								if (nm_parts.Length > 0) f_name = CheckDot(nm_parts[0]);
								if (nm_parts.Length > 1) f_pat = CheckDot(nm_parts[1]);
								if (nm_parts.Length > 2) f_fam = CheckDot(nm_parts[2]);

								GEDCOMIndividualRecord sp = this.fBase.Context.CreatePersonEx(f_name, f_pat, f_fam, sx, false);
								fam.AddSpouse(sp);
							}
						}
						catch (Exception ex)
						{
							this.fBase.Host.LogWrite("Importer.CheckSpouses(): " + ex.Message);
						}
					}
				}
			}
		}

		private void Import_StringList(StringList content)
		{
			this.fLog.Clear();

			StringList buf = new StringList();
			this.fPersonsList = new StringList();
			try
			{
				int prev_id = 0;
				GEDCOMIndividualRecord i_rec = null;

				int num = content.Count;
				for (int i = 0; i < num; i++)
				{
					string s = content[i].Trim();

					if (s == "")
					{
						this.CheckBuf(buf, i_rec);
						i_rec = null;
					}
					else
					{
						if (IsRomeLine(s))
						{
							this.fLog.Add("> " + fLangMan.LS(ILS.LSID_Generation) + " \"" + s + "\"");
							i_rec = null;
						}
						else
						{
							string p_id = "";
							if (!IsPersonLine(s, ref p_id))
							{
								buf.Add(s);
							}
							else
							{
								this.CheckBuf(buf, i_rec);
								int self_id = 0;
								i_rec = this.ParsePerson(buf, s, p_id, ref self_id);

								this.fLog.Add("> " + fLangMan.LS(ILS.LSID_PersonParsed) + " \"" + p_id + "\".");

								if (self_id - prev_id > 1)
								{
									this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_LineSeq));
								}

								prev_id = self_id;
							}
						}
					}
				}
			}
			finally
			{
                this.fPersonsList.Dispose();
                buf.Dispose();
			}
		}

		public void TreeImportEx(string fileName)
		{
			string ext = Path.GetExtension(fileName).ToLower();

			if (ext == ".txt")
			{
				this.Import_PlainText(fileName);
			}
			else if (ext == ".doc")
			{
				this.Import_Word(fileName);
			}
			else if (ext == ".xls")
			{
				this.Import_Excel(fileName);
			}
			else
			{
				throw new ImporterException(fLangMan.LS(ILS.LSID_FormatUnsupported));
			}
		}
	}
}
