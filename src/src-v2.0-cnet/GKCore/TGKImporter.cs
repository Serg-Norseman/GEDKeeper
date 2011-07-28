using GedCom551;
using GKCore;
using GKUI;
using GKSys;
using System;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKCore
{
	[StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
	internal struct _Unnamed7
	{
		internal static readonly byte[] Romes;

		static _Unnamed7()
		{
			// alert!!!
			/*byte[] expr_11 = BDSSystem.SetNew(32);
			RuntimeHelpers.InitializeArray(expr_11, ldtoken(GKL._Set_val_3));
			_Unnamed7.Romes = expr_11;*/
		}
	}

	[TSetElementType(typeof(AnsiChar))]
	internal class _Unnamed8 : TUniqueTypeModifier
	{
	}

	public class TGKImporter
	{
		internal TGenEngine FEngine;
		internal TGEDCOMTree FTree;
		internal ListBox.ObjectCollection FLog;
		internal TStringList FPersonsList;

		internal void AddChild(TGEDCOMIndividualRecord parent, TGEDCOMIndividualRecord child, int mar_id)
		{
			if (mar_id < 0)
			{
				mar_id = 1;
			}
			if (mar_id > 1)
			{
				object obj = mar_id;
				int num = TGKSys.Hole(ref obj);
				mar_id = (int)obj;
			}
			TGEDCOMObject.TGEDCOMSex sex = parent.Sex;
			if (sex == TGEDCOMObject.TGEDCOMSex.svNone || sex == TGEDCOMObject.TGEDCOMSex.svUndetermined)
			{
				parent.Sex = TGEDCOMObject.TGEDCOMSex.svMale;
			}
			while (parent.SpouseToFamilyLinksCount < mar_id)
			{
				this.AddFamily(parent);
			}
			mar_id--;
			TGEDCOMFamilyRecord family = parent.GetSpouseToFamilyLink(mar_id).Family;
			TGEDCOMPointer ptr = new TGEDCOMPointer(this.FTree, family, "", "");
			ptr.SetNamedValue("CHIL", child);
			family.AddChild(ptr);
			TGEDCOMChildToFamilyLink chLink = new TGEDCOMChildToFamilyLink(this.FTree, child, "", "");
			chLink.Family = family;
			child.AddChildToFamilyLink(chLink);
		}
		internal TGEDCOMFamilyRecord AddFamily(TGEDCOMIndividualRecord parent)
		{
			TGEDCOMFamilyRecord Result = TGenEngine.CreateFamilyEx(this.FTree);
			this.FEngine.AddFamilySpouse(Result, parent);
			return Result;
		}

		internal string CheckDot(string aStr)
		{
			if (aStr[((aStr != null) ? aStr.Length : 0) - 1] == '.')
			{
				aStr = BDSSystem.WStrCopy(aStr, 1, ((aStr != null) ? aStr.Length : 0) - 1);
			}
			return aStr.Trim();
		}

		internal void DefinePersonName([In] string aStr, [In] string p_id, ref string f_name, ref string f_pat, ref string f_fam, ref string bd, ref string dd)
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
			int b_pos = BDSSystem.Pos("*", tmp);
			int d_pos = BDSSystem.Pos("+", tmp);
			if (d_pos > 0 && d_pos > b_pos)
			{
				dd = BDSSystem.WStrCopy(tmp, d_pos + 1, ((tmp != null) ? tmp.Length : 0) - d_pos);
				string text = dd;
				int num2 = ((text != null) ? text.Length : 0) + 1;
				tmp = tmp.Remove(d_pos - 1, num2);
				tmp = tmp.Trim();
			}
			if (b_pos > 0)
			{
				bd = BDSSystem.WStrCopy(tmp, b_pos + 1, ((tmp != null) ? tmp.Length : 0) - b_pos);
				string text2 = bd;
				int num3 = ((text2 != null) ? text2.Length : 0) + 1;
				tmp = tmp.Remove(b_pos - 1, num3);
				tmp = tmp.Trim();
			}
			tmp = tmp.Trim();
			int toks = TGKSys.GetTokensCount(tmp, ' ');
			if (toks > 0)
			{
				f_name = this.CheckDot(TGKSys.GetToken(tmp, ' ', 1));
			}
			if (toks > 1)
			{
				f_pat = this.CheckDot(TGKSys.GetToken(tmp, ' ', 2));
			}
			if (toks > 2)
			{
				f_fam = this.CheckDot(TGKSys.GetToken(tmp, ' ', 3));
			}
		}
		internal string DeleteBlanks([In] string S)
		{
			string Result = S;
			int I = 1;
			while (I <= ((Result != null) ? Result.Length : 0))
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
			return Result;
		}
		internal string ExtractNumComment([In] string S, ref string Comment, bool NoException)
		{
			string Result = S;
			Comment = "";
			Result = Result.Remove(0, 1);
			int I = 0;
			while (I < ((Result != null) ? Result.Length : 0) && Result[I + 1 - 1] != ')')
			{
				I++;
			}
			if (I > 0)
			{
				Comment = BDSSystem.WStrCopy(Result, 1, I);
				Result = Result.Remove(0, I);
			}
			Result = Result.Remove(0, 1);
			return Result;
		}
		internal bool IsPersonLine([In] string aStr, ref string p_id)
		{
			int i = 1;
			p_id = "";
			while (i <= ((aStr != null) ? aStr.Length : 0))
			{
				char c = aStr[i - 1];
				if (c != ' ' && (c < '(' || (c >= '*' && c != '-' && (c < '/' || (c >= ':' && c != '?')))))
				{
					break;
				}
				p_id += aStr[i - 1];
				i++;
				if (aStr[i - 1] == '(')
				{
					object obj = i;
					int num = TGKSys.Hole(ref obj);
					i = (int)obj;
					while (i <= ((aStr != null) ? aStr.Length : 0) && aStr[i - 1] != ')')
					{
						p_id += aStr[i - 1];
						i++;
					}
				}
			}
			return BDSSystem.WStrCmp(p_id, "") != 0 && aStr[i - 1] == '.' && aStr[i + 1 - 1] == ' ';
		}
		internal bool IsRomeLine([In] string aStr)
		{
			int i = 1;
			string rs = "";
			while (i <= ((aStr != null) ? aStr.Length : 0) && BDSSystem.SetTest(_Unnamed7.Romes, (int)aStr[i - 1], 32))
			{
				rs += aStr[i - 1];
				i++;
			}
			return BDSSystem.WStrCmp(rs, "") != 0 && BDSSystem.WStrCmp(rs, aStr) == 0;
		}
		internal void SetEvent(TGEDCOMIndividualRecord iRec, string evName, string date)
		{
			int[] val = new int[3];
			TGEDCOMCustomEvent ev = TGenEngine.CreateEventEx(this.FTree, iRec, evName, "", "");
			try
			{
				string prefix = "";
				if (BDSSystem.Pos("п.", date) == 1)
				{
					prefix = "AFT ";
					date = date.Remove(0, 2);
					date = date.Trim();
				}
				else
				{
					if (BDSSystem.Pos("до", date) == 1)
					{
						prefix = "BEF ";
						date = date.Remove(0, 3);
						date = date.Trim();
					}
				}
				string tmp = "";
				int toks = TGKSys.GetTokensCount(date, '.');
				if (toks > 3)
				{
					throw new Exception("date failed");
				}
				string ym = "";
				int arg_A7_0 = 1;
				int num = toks;
				int i = arg_A7_0;
				if (num >= i)
				{
					num++;
					do
					{
						tmp = TGKSys.GetToken(date, '.', i);
						int x = BDSSystem.Pos("/", tmp);
						if (x > 0)
						{
							ym = BDSSystem.WStrCopy(tmp, x + 1, ((tmp != null) ? tmp.Length : 0) - x);
							int num2 = ((ym != null) ? ym.Length : 0) + 1;
							tmp = tmp.Remove(x - 1, num2);
						}
						val[i - 1] = int.Parse(tmp);
						i++;
					}
					while (i != num);
				}
				if (toks != 1)
				{
					if (toks != 2)
					{
						if (toks == 3)
						{
							tmp = string.Concat(new string[]
							{
								val[0].ToString(), 
								" ", 
								TGEDCOMDate.GEDCOMMonthArray[val[1] - 1], 
								" ", 
								val[2].ToString()
							});
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
				if (BDSSystem.WStrCmp(ym, "") != 0)
				{
					tmp = tmp + "/" + ym;
				}
				ev.Detail.Date.ParseString(tmp);
			}
			catch (Exception E)
			{
				this.FLog.Add(string.Concat(new string[]
				{
					">>>> ", 
					GKL.LSList[401], 
					" \"", 
					date, 
					"\""
				}));
			}
		}
		internal TGEDCOMIndividualRecord ParsePerson(TStringList buf, string aStr, string p_id, ref int self_id)
		{
			self_id = -1;
			int parent_id = -1;
			int mar_id = -1;
			string S = this.DeleteBlanks(p_id);
			S = TGEDCOMObject.ExtractNumber(S, ref self_id, true, -1);
			if (BDSSystem.WStrCmp(S, "") != 0 && S[0] == '-')
			{
				S = S.Remove(0, 1);
				S = TGEDCOMObject.ExtractNumber(S, ref parent_id, true, -1);
				if (BDSSystem.WStrCmp(S, "") != 0 && S[0] == '(')
				{
					string com = "";
					S = this.ExtractNumComment(S, ref com, true);
				}
				if (BDSSystem.WStrCmp(S, "") != 0 && S[0] == '/')
				{
					S = S.Remove(0, 1);
					S = TGEDCOMObject.ExtractNumber(S, ref mar_id, true, -1);
				}
			}
			string f_name = "";
			string f_pat = "";
			string f_fam = "";
			string bd = "";
			string dd = "";
			this.DefinePersonName(aStr, p_id, ref f_name, ref f_pat, ref f_fam, ref bd, ref dd);
			TGEDCOMIndividualRecord Result = TGenEngine.CreatePersonEx(this.FTree, f_name, f_pat, f_fam, TGEDCOMObject.TGEDCOMSex.svNone, false);
			TfmSexCheck.CheckPersonSex(Result, GKL.fmGEDKeeper.NamesTable);
			this.FPersonsList.AddObject(self_id.ToString(), Result);
			buf.Add(aStr);
			if (BDSSystem.WStrCmp(bd, "") != 0)
			{
				this.SetEvent(Result, "BIRT", bd);
			}
			if (BDSSystem.WStrCmp(dd, "") != 0)
			{
				this.SetEvent(Result, "DEAT", dd);
			}
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
					this.FLog.Add(string.Concat(new string[]
					{
						">>>> ", 
						GKL.LSList[400], 
						" \"", 
						parent_id.ToString(), 
						"\"."
					}));
				}
			}
			return Result;
		}
		internal void Import_PlainText(string aFileName)
		{
			try
			{
				TStringList content = new TStringList();
				try
				{
					content.LoadFromFile(aFileName);
					content.Add("");
					this.Import_StringList(content);
				}
				finally
				{
					content.Free();
				}
			}
			catch (Exception E)
			{
				this.FLog.Add(">>>> " + GKL.LSList[396]);
				TGKSys.LogWrite("Import_PlainText(): " + E.Message);
			}
		}
		internal void Import_Excel(string aFileName)
		{
			try
			{
			}
			catch (Exception E)
			{
				this.FLog.Add(">>>> " + GKL.LSList[396]);
				TGKSys.LogWrite("Import_Excel(): " + E.Message);
			}
		}
		internal void Import_Word(string aFileName)
		{
			try
			{
			}
			catch (Exception E)
			{
				this.FLog.Add(">>>> " + GKL.LSList[396]);
				TGKSys.LogWrite("Import_Word(): " + E.Message);
			}
		}
		internal void CheckBuf(TStringList buf, TGEDCOMIndividualRecord iRec)
		{
			if (BDSSystem.WStrCmp(buf.Text, "") != 0)
			{
				if (iRec != null)
				{
					this.CheckSpouses(buf, iRec);
				}
				TGenEngine.CreateNoteEx(this.FTree, buf, iRec);
				buf.Clear();
			}
		}
		internal void CheckSpouses(TStringList buf, TGEDCOMIndividualRecord iRec)
		{
			int arg_0A_0 = 0;
			int num2 = buf.Count - 1;
			int i = arg_0A_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					string s = buf[i];
					while (((s != null) ? s.Length : 0) > 0)
					{
						char c = s[0];
						if (c != ' ' && c != '.')
						{
							break;
						}
						s = s.Remove(0, 1);
					}
					char c2 = s[0];
					if (c2 == '\u0016' || c2 == '\u001c')
					{
						char c3 = s[1];
						if (c3 != ' ')
						{
							if (c3 < '1' || c3 >= ':')
							{
								goto IL_2B5;
							}
						}
						try
						{
							TGEDCOMObject.TGEDCOMSex sx;
							if (s[0] == 'М')
							{
								sx = TGEDCOMObject.TGEDCOMSex.svMale;
							}
							else
							{
								sx = TGEDCOMObject.TGEDCOMSex.svFemale;
							}
							s = s.Remove(0, 1);
							int num = 0;
							s = TGEDCOMObject.ExtractNumber(s, ref num, true, 1);
							while (((s != null) ? s.Length : 0) > 0 && s[0] == ' ')
							{
								s = s.Remove(0, 1);
							}
							int p;
							if (s[0] == '(')
							{
								p = 0;
								while (p < ((s != null) ? s.Length : 0) && s[p + 1 - 1] != ')')
								{
									p++;
								}
								if (p > 0)
								{
									p++;
									string mar_date = BDSSystem.WStrCopy(s, 1, p);
									mar_date = mar_date.Trim();
									s = s.Remove(0, p);
								}
							}
							while (((s != null) ? s.Length : 0) > 0)
							{
								char c4 = s[0];
								if (c4 != '\u0013' && c4 != ' ' && c4 != '-')
								{
									break;
								}
								s = s.Remove(0, 1);
							}
							for (p = 0; p < ((s != null) ? s.Length : 0); p++)
							{
								char c5 = s[p + 1 - 1];
								if (c5 >= '*' && (c5 < ',' || c5 == '.'))
								{
									break;
								}
							}
							string name = "";
							if (p > 0)
							{
								name = BDSSystem.WStrCopy(s, 1, p);
								name = name.Trim();
								s = s.Remove(0, p);
							}
							if (BDSSystem.WStrCmp(name, "") != 0)
							{
								TGEDCOMFamilyRecord fam = this.AddFamily(iRec);
								name = name.Trim();
								p = TGKSys.GetTokensCount(name, ' ');
								string f_name = "";
								if (p > 0)
								{
									f_name = this.CheckDot(TGKSys.GetToken(name, ' ', 1));
								}
								string f_pat = "";
								if (p > 1)
								{
									f_pat = this.CheckDot(TGKSys.GetToken(name, ' ', 2));
								}
								string f_fam = "";
								if (p > 2)
								{
									f_fam = this.CheckDot(TGKSys.GetToken(name, ' ', 3));
								}
								TGEDCOMIndividualRecord sp = TGenEngine.CreatePersonEx(this.FTree, f_name, f_pat, f_fam, sx, false);
								this.FEngine.AddFamilySpouse(fam, sp);
							}
						}
						catch (Exception E)
						{
							//
						}
					}
					IL_2B5:
					i++;
				}
				while (i != num2);
			}
		}
		internal void Import_StringList(TStringList aContent)
		{
			this.FLog.Clear();
			TStringList buf = new TStringList();
			this.FPersonsList = new TStringList();
			try
			{
				int prev_id = 0;
				TGEDCOMIndividualRecord i_rec = null;
				int arg_2B_0 = 0;
				int num = aContent.Count - 1;
				int i = arg_2B_0;
				if (num >= i)
				{
					num++;
					do
					{
						string s = aContent[i].Trim();
						if (BDSSystem.WStrCmp(s, "") == 0)
						{
							this.CheckBuf(buf, i_rec);
							i_rec = null;
						}
						else
						{
							if (this.IsRomeLine(s))
							{
								this.FLog.Add(string.Concat(new string[]
								{
									"> ", 
									GKL.LSList[399], 
									" \"", 
									s, 
									"\""
								}));
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
									this.FLog.Add(string.Concat(new string[]
									{
										"> ", 
										GKL.LSList[398], 
										" \"", 
										p_id, 
										"\"."
									}));
									if (self_id - prev_id > 1)
									{
										this.FLog.Add(">>>> " + GKL.LSList[397]);
									}
									prev_id = self_id;
								}
							}
						}
						i++;
					}
					while (i != num);
				}
			}
			finally
			{
				this.FPersonsList.Free();
				buf.Free();
			}
		}
		public TGKImporter(TGenEngine aEngine, ListBox.ObjectCollection aLog)
		{
			this.FEngine = aEngine;
			this.FTree = this.FEngine.Tree;
			this.FLog = aLog;
		}
		public void TreeImportEx(string aFileName)
		{
			string E = Path.GetExtension(aFileName).ToLower();
			if (BDSSystem.WStrCmp(E, ".txt") == 0)
			{
				this.Import_PlainText(aFileName);
			}
			else
			{
				if (BDSSystem.WStrCmp(E, ".csv") != 0)
				{
					if (BDSSystem.WStrCmp(E, ".doc") == 0)
					{
						this.Import_Word(aFileName);
					}
					else
					{
						if (BDSSystem.WStrCmp(E, ".xls") != 0)
						{
							throw new Exception(GKL.LSList[395]);
						}
						this.Import_Excel(aFileName);
					}
				}
			}
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

	}
}
