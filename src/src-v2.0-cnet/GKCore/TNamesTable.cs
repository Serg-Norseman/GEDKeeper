using GedCom551;
using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace GKCore
{
	public class TNamesTable : IDisposable
	{

		public class TName
		{
			public string Name;
			public string F_Patronymic;
			public string M_Patronymic;
			public TGEDCOMObject.TGEDCOMSex Sex;

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		internal TObjectList FNames;

		protected internal bool Disposed_;

		[Browsable(false)]
		public int NameCount
		{
			get { return this.GetNameCount(); }
		}

		/*[Browsable(false)]
		public TNamesTable.TName Names
		{
			get
			{
				return this.GetName(Index);
			}
		}*/

		internal bool Comparable(string aName, string aPatronymic)
		{
			int cmp = 0;
			int len = Math.Min((aName != null) ? aName.Length : 0, (aPatronymic != null) ? aPatronymic.Length : 0);
			int arg_23_0 = 1;
			int num = len;
			int i = arg_23_0;
			if (num >= i)
			{
				num++;
				while (aName[i - 1] == aPatronymic[i - 1])
				{
					cmp++;
					i++;
					if (i == num)
					{
						break;
					}
				}
			}
			return (long)cmp >= checked((long)Math.Round((double)unchecked(len * 3) / 4.0));
		}

		internal TNamesTable.TName GetName(int Index)
		{
			TNamesTable.TName Result;
			if (Index >= 0 && Index < this.FNames.Count)
			{
				Result = (this.FNames[Index] as TNamesTable.TName);
			}
			else
			{
				Result = null;
			}
			return Result;
		}

		internal int GetNameCount()
		{
			return this.FNames.Count;
		}

		public TNamesTable()
		{
			this.FNames = new TObjectList(true);
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FNames.Free();
				this.Disposed_ = true;
			}
		}

		public void ImportNames(TGEDCOMTree aTree)
		{
			try
			{
				int num = aTree.RecordsCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						if (aTree.GetRecord(i) is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)aTree.GetRecord(i);
							string dummy = "";
							string ch_name = "";
							string ch_pat = "";
							TGenEngine.GetNameParts(iRec, ref dummy, ref ch_name, ref ch_pat);
							this.SetNameSex(ch_name, iRec.Sex);
							if (iRec.ChildToFamilyLinksCount != 0)
							{
								TGEDCOMFamilyRecord family = iRec.GetChildToFamilyLink(0).Family;
								if (family != null)
								{
									TGEDCOMIndividualRecord iFather = family.Husband.Value as TGEDCOMIndividualRecord;
									if (iFather != null)
									{
										string fat_nam = "";
										TGenEngine.GetNameParts(iFather, ref dummy, ref fat_nam, ref dummy);
										if (((ch_pat != null) ? ch_pat.Length : 0) > 1 && ((fat_nam != null) ? fat_nam.Length : 0) > 1 && this.Comparable(fat_nam, ch_pat))
										{
											this.SetName(fat_nam, ch_pat, iRec.Sex);
										}
									}
								}
							}
						}
						i++;
					}
					while (i != num);
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("TNamesTable.ImportNames(): " + E.Message);
			}
		}

		public void LoadFromFile([In] string aFileName)
		{
			if (File.Exists(aFileName))
			{
				StreamReader strd = new StreamReader(aFileName, Encoding.GetEncoding(1251));
				try
				{
					while (strd.Peek() != -1)
					{
						string st = strd.ReadLine().Trim();
						string[] data = st.Split(new char[] { ';' });
						TNamesTable.TName nm = new TNamesTable.TName();
						nm.Name = data[0];
						nm.F_Patronymic = data[1];
						nm.M_Patronymic = data[2];
						if (data[3] != "")
						{
							nm.Sex = TGenEngine.GetSexBySign(data[3][0]);
						}
						this.FNames.Add(nm);
					}
				}
				finally
				{
					strd.Close();
				}
			}
		}

		public void SaveToFile([In] string aFileName)
		{
			StreamWriter strd = new StreamWriter(aFileName, false, Encoding.GetEncoding(1251));
			try
			{
				int num = this.FNames.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TNamesTable.TName nm = this.GetName(i);
						string st = string.Concat(new string[]
						{
							nm.Name, 
							";", 
							nm.F_Patronymic, 
							";", 
							nm.M_Patronymic, 
							";", 
							TGenEngine.SexData[(int)nm.Sex].Sign
						});
						strd.WriteLine(st);
						i++;
					}
					while (i != num);
				}
			}
			finally
			{
				strd.Close();
			}
		}

		public TNamesTable.TName AddName(string aName)
		{
			TNamesTable.TName Result = new TNamesTable.TName();
			Result.Name = aName;
			this.FNames.Add(Result);
			return Result;
		}

		public TNamesTable.TName FindName(string aName)
		{
			TNamesTable.TName Result = null;
			int num = this.FNames.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				TNamesTable.TName j;
				while (true)
				{
					j = (this.FNames[i] as TNamesTable.TName);
					if (j.Name == aName)
					{
						break;
					}
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = j;
			}
			return Result;
		}

		public string GetPatronymicByName(string aName, TGEDCOMObject.TGEDCOMSex aSex)
		{
			string Result = "";
			TNamesTable.TName i = this.FindName(aName);
			if (i != null)
			{
				if (aSex != TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (aSex == TGEDCOMObject.TGEDCOMSex.svFemale)
					{
						Result = i.F_Patronymic;
					}
				}
				else
				{
					Result = i.M_Patronymic;
				}
			}
			return Result;
		}

		public string GetNameByPatronymic(string aPatronymic, TGEDCOMObject.TGEDCOMSex aSex)
		{
			string Result = "";
			if (aPatronymic != "")
			{
				int num = this.FNames.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					TNamesTable.TName j;
					while (true)
					{
						j = (this.FNames[i] as TNamesTable.TName);
						if (j.F_Patronymic == aPatronymic || j.M_Patronymic == aPatronymic)
						{
							break;
						}
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = j.Name;
				}
			}
			return Result;
		}

		public TGEDCOMObject.TGEDCOMSex GetSexByName(string aName)
		{
			TNamesTable.TName i = this.FindName(aName);
			TGEDCOMObject.TGEDCOMSex Result;
			if (i == null)
			{
				Result = TGEDCOMObject.TGEDCOMSex.svNone;
			}
			else
			{
				Result = i.Sex;
			}
			return Result;
		}

		public void SetName(string aName, string aPatronymic, TGEDCOMObject.TGEDCOMSex aSex)
		{
			if (aName != "")
			{
				TNamesTable.TName i = this.FindName(aName);
				if (i == null)
				{
					i = this.AddName(aName);
				}
				if (aSex != TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (aSex == TGEDCOMObject.TGEDCOMSex.svFemale)
					{
						if (i.F_Patronymic == "")
						{
							i.F_Patronymic = aPatronymic;
						}
					}
				}
				else
				{
					if (i.M_Patronymic == "")
					{
						i.M_Patronymic = aPatronymic;
					}
				}
			}
		}

		public void SetNameSex(string aName, TGEDCOMObject.TGEDCOMSex aSex)
		{
			if (aName != "")
			{
				TNamesTable.TName i = this.FindName(aName);
				if (i == null)
				{
					i = new TNamesTable.TName();
					i.Name = aName;
					this.FNames.Add(i);
				}

				if (i.Sex == TGEDCOMObject.TGEDCOMSex.svNone && aSex >= TGEDCOMObject.TGEDCOMSex.svMale && aSex < TGEDCOMObject.TGEDCOMSex.svUndetermined)
				{
					i.Sex = aSex;
				}
			}
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
