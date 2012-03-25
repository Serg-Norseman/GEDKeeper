using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKUI;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public class Importer
	{
		private TGenEngine FEngine;
		private TGEDCOMTree FTree;
		private ListBox.ObjectCollection FLog;
		private StringList FPersonsList;

		private void AddChild(TGEDCOMIndividualRecord parent, TGEDCOMIndividualRecord child, int mar_id)
		{
			if (mar_id < 0)
			{
				mar_id = 1;
			}

			if (mar_id > 1)
			{
				// ???
			}

			TGEDCOMSex sex = parent.Sex;
			if (sex == TGEDCOMSex.svNone || sex == TGEDCOMSex.svUndetermined)
			{
				parent.Sex = TGEDCOMSex.svMale;
			}

			while (parent.SpouseToFamilyLinks.Count < mar_id)
			{
				this.AddFamily(parent);
			}
			mar_id--;

			TGEDCOMFamilyRecord family = parent.SpouseToFamilyLinks[mar_id].Family;
			TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, family, "", "");
			ptr.SetNamedValue("CHIL", child);
			family.Childrens.Add(ptr);

			TGEDCOMChildToFamilyLink chLink = new TGEDCOMChildToFamilyLink(this.FTree, child, "", "");
			chLink.Family = family;
			child.ChildToFamilyLinks.Add(chLink);
		}

		private TGEDCOMFamilyRecord AddFamily(TGEDCOMIndividualRecord parent)
		{
			TGEDCOMFamilyRecord Result = TGenEngine.CreateFamilyEx(this.FTree);
			this.FEngine.AddFamilySpouse(Result, parent);
			return Result;
		}

		private string CheckDot(string aStr)
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

		private void DefinePersonName([In] string aStr, [In] string p_id, ref string f_name, ref string f_pat, ref string f_fam, ref string bd, ref string dd)
		{
			f_name = "";
			f_pat = "";
			f_fam = "";
			bd = "";
			dd = "";
			string tmp = aStr;
			int num = ((p_id != null) ? p_id.Length : 0) + 2;
			tmp = tmp.Remove(0, num);
			tmp = this.CheckDot(tmp);

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
			if (tokens.Length > 0) f_name = this.CheckDot(tokens[0]);
			if (tokens.Length > 1) f_pat = this.CheckDot(tokens[1]);
			if (tokens.Length > 2) f_fam = this.CheckDot(tokens[2]);
		}

		private string DeleteBlanks([In] string S)
		{
			string Result = S;
			if (Result != null)
			{
				int I = 1;
				while (I <= Result.Length)
				{
					if (Result[I - 1] == ' ')
					{
						Result = Result.Remove(I - 1, 1);
					}
					else
					{
						I++;
					}
				}
			}
			return Result;
		}

		private string ExtractNumComment([In] string S, ref string Comment, bool NoException)
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

		private bool IsPersonLine([In] string aStr, ref string p_id)
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

		private bool IsRomeChar(char c)
		{
			return (c == 'I' || c == 'V' || c == 'X' || c == 'L' || c == 'C' || c == 'D' || c == 'M');
		}

		private bool IsRomeLine([In] string aStr)
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

		private void SetEvent(TGEDCOMIndividualRecord iRec, string evName, string date)
		{
			int[] val = new int[3];
			TGEDCOMCustomEvent ev = TGenEngine.CreateEventEx(this.FTree, iRec, evName, "", "");
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
					throw new Exception("date failed");
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
							tmp = val[0].ToString() + " " + TGEDCOMDate.GEDCOMMonthArray[val[1] - 1] + " " + val[2].ToString();
						}
					}
					else
					{
						tmp = TGEDCOMDate.GEDCOMMonthArray[val[0] - 1] + " " + val[1].ToString();
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
			catch (Exception E)
			{
				this.FLog.Add(">>>> " + LangMan.LSList[401] + " \"" + date + "\"");
				SysUtils.LogWrite("TGKImporter.SetEvent(" + date + "): " + E.Message);
			}
		}

		private TGEDCOMIndividualRecord ParsePerson(StringList buf, string aStr, string p_id, ref int self_id)
		{
			self_id = -1;
			int parent_id = -1;
			int mar_id = -1;

			string S = this.DeleteBlanks(p_id);
			S = TGEDCOMObject.ExtractNumber(S, out self_id, true, -1);

			if (S != "" && S[0] == '-')
			{
				S = S.Remove(0, 1);
				S = TGEDCOMObject.ExtractNumber(S, out parent_id, true, -1);

				if (S != "" && S[0] == '(')
				{
					string com = "";
					S = this.ExtractNumComment(S, ref com, true);
				}

				if (S != "" && S[0] == '/')
				{
					S = S.Remove(0, 1);
					S = TGEDCOMObject.ExtractNumber(S, out mar_id, true, -1);
				}
			}

			string f_name = "";
			string f_pat = "";
			string f_fam = "";
			string bd = "";
			string dd = "";

			this.DefinePersonName(aStr, p_id, ref f_name, ref f_pat, ref f_fam, ref bd, ref dd);
			TGEDCOMIndividualRecord Result = TGenEngine.CreatePersonEx(this.FTree, f_name, f_pat, f_fam, TGEDCOMSex.svNone, false);
			TfmSexCheck.CheckPersonSex(Result, GKUI.TfmGEDKeeper.Instance.NamesTable);
			this.FPersonsList.AddObject(self_id.ToString(), Result);

			buf.Add(aStr);

			if (bd != "") this.SetEvent(Result, "BIRT", bd);
			if (dd != "") this.SetEvent(Result, "DEAT", dd);

			if (parent_id > 0)
			{
				int x = this.FPersonsList.IndexOf(parent_id.ToString());
				if (x >= 0)
				{
					TGEDCOMIndividualRecord parent = this.FPersonsList.GetObject(x) as TGEDCOMIndividualRecord;
					this.AddChild(parent, Result, mar_id);
				}
				else
				{
					this.FLog.Add(">>>> " + LangMan.LSList[400] + " \"" + parent_id.ToString() + "\".");
				}
			}
			return Result;
		}

		public void Import_PlainText(string aFileName)
		{
			try
			{
				StreamReader strd = new StreamReader(aFileName, Encoding.GetEncoding(1251));
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
					content.Free();
				}
			}
			catch (Exception E)
			{
				this.FLog.Add(">>>> " + LangMan.LSList[396]);
				SysUtils.LogWrite("Import_PlainText(): " + E.Message);
			}
		}

		public void Import_Excel(string aFileName)
		{
			try
			{
			}
			catch (Exception E)
			{
				this.FLog.Add(">>>> " + LangMan.LSList[396]);
				SysUtils.LogWrite("Import_Excel(): " + E.Message);
			}
		}

		public void Import_Word(string aFileName)
		{
			try
			{
			}
			catch (Exception E)
			{
				this.FLog.Add(">>>> " + LangMan.LSList[396]);
				SysUtils.LogWrite("Import_Word(): " + E.Message);
			}
		}

		private void CheckBuf(StringList buf, TGEDCOMIndividualRecord iRec)
		{
			if (buf.Text != "")
			{
				if (iRec != null)
				{
					this.CheckSpouses(buf, iRec);
				}
				TGenEngine.CreateNoteEx(this.FTree, buf, iRec);
				buf.Clear();
			}
		}

		private bool IsSpouseLine([In] string S, out TGEDCOMSex spouseSex, out int spouseNum)
		{
			bool res = false;
			spouseNum = 1;
			spouseSex = TGEDCOMSex.svNone;

			char sym = S[0];
			if (sym == 'М' || sym == 'Ж')
			{
				char c2 = S[1];
				char c3 = S[2];
				char c4 = S[3];
				char c5 = S[4];

				if (sym == 'М') {
					spouseSex = TGEDCOMSex.svMale;
				} else {
					spouseSex = TGEDCOMSex.svFemale;
				}

				bool res1 = (c2 == ' ' && c3 == '–' && c4 == ' ');
				bool res2 = (c2 >= '1' && c2 <= '9' && c3 == ' ' && c4 == '–' && c5 == ' ');

				if (res1)
				{
					res = true;
				}

				if (res2)
				{
					res = true;
					int.TryParse("0"+c2, out spouseNum);
				}
			}

			return res;
		}

		private void CheckSpouses(StringList buf, TGEDCOMIndividualRecord iRec)
		{
			int num2 = buf.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				string s = buf[i];
				if (string.IsNullOrEmpty(s)) continue;

				while (s.Length > 0 && (s[0] == ' ' || s[0] == '.'))
				{
					s = s.Remove(0, 1);
				}

				if (s.Length > 2)
				{
					char c2 = s[0];
					char c3 = s[1];
					if ((c2 == 'М' || c2 == 'Ж') && ((c3 == ' ') || (c3 >= '1' && c3 <= '9')))
					{
						try
						{
							// define sex
							TGEDCOMSex sx;
							if (s[0] == 'М') {
								sx = TGEDCOMSex.svMale;
							} else {
								sx = TGEDCOMSex.svFemale;
							}
							s = s.Remove(0, 1);

							// number of spouse
							int num;
							s = TGEDCOMObject.ExtractNumber(s, out num, true, 1);

							// skip blanks
							while (s.Length > 0 && s[0] == ' ') s = s.Remove(0, 1);

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
							while (s.Length > 0 && (s[0] == '–' || s[0] == ' ' || s[0] == '-'))
							{
								s = s.Remove(0, 1);
							}

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
								TGEDCOMFamilyRecord fam = this.AddFamily(iRec);

								string f_name = "";
								string f_pat = "";
								string f_fam = "";
								string[] nm_parts = name.Trim().Split(' ');
								if (nm_parts.Length > 0) f_name = this.CheckDot(nm_parts[0]);
								if (nm_parts.Length > 1) f_pat = this.CheckDot(nm_parts[1]);
								if (nm_parts.Length > 2) f_fam = this.CheckDot(nm_parts[2]);

								TGEDCOMIndividualRecord sp = TGenEngine.CreatePersonEx(this.FTree, f_name, f_pat, f_fam, sx, false);
								this.FEngine.AddFamilySpouse(fam, sp);
							}
						}
						catch (Exception E)
						{
							SysUtils.LogWrite("TGKImporter.CheckSpouses(): " + E.Message);
						}
					}
				}
			}
		}

		private void Import_StringList(StringList aContent)
		{
			this.FLog.Clear();
			StringList buf = new StringList();
			this.FPersonsList = new StringList();
			try
			{
				int prev_id = 0;
				TGEDCOMIndividualRecord i_rec = null;

				int num = aContent.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					string s = aContent[i].Trim();
					if (s == "")
					{
						this.CheckBuf(buf, i_rec);
						i_rec = null;
					}
					else
					{
						if (this.IsRomeLine(s))
						{
							this.FLog.Add("> " + LangMan.LSList[399] + " \"" + s + "\"");
							i_rec = null;
						}
						else
						{
							string p_id = "";
							if (!this.IsPersonLine(s, ref p_id))
							{
								buf.Add(s);
							}
							else
							{
								this.CheckBuf(buf, i_rec);
								int self_id = 0;
								i_rec = this.ParsePerson(buf, s, p_id, ref self_id);

								this.FLog.Add("> " + LangMan.LSList[398] + " \"" + p_id + "\".");

								if (self_id - prev_id > 1)
								{
									this.FLog.Add(">>>> " + LangMan.LSList[397]);
								}

								prev_id = self_id;
							}
						}
					}
				}
			}
			finally
			{
				this.FPersonsList.Free();
				buf.Free();
			}
		}

		public Importer(TGenEngine aEngine, ListBox.ObjectCollection aLog)
		{
			this.FEngine = aEngine;
			this.FTree = this.FEngine.Tree;
			this.FLog = aLog;
		}

		public void TreeImportEx(string aFileName)
		{
			string E = Path.GetExtension(aFileName).ToLower();
			if (E == ".txt")
			{
				this.Import_PlainText(aFileName);
			}
			else
			{
				if (E != ".csv")
				{
					if (E == ".doc")
					{
						this.Import_Word(aFileName);
					}
					else
					{
						if (E != ".xls")
						{
							throw new Exception(LangMan.LSList[395]);
						}
						this.Import_Excel(aFileName);
					}
				}
			}
		}

		public void Free()
		{
			SysUtils.Free(this);
		}

	}
}
