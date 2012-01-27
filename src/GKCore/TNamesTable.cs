using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using GedCom551;
using GKSys;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKCore
{
	public class TNamesTable : IDisposable
	{
		public class TName
		{
			public string Name;
			public string F_Patronymic;
			public string M_Patronymic;
			public TGEDCOMSex Sex;
		}

		private Hashtable FNames;
		protected bool Disposed_;

		public TNamesTable()
		{
			this.FNames = new Hashtable();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				//this.FNames.Dispose();
				this.Disposed_ = true;
			}
		}

		private bool Comparable(string aName, string aPatronymic)
		{
			if (aName == null || aPatronymic == null)
			{
				return false;
			}
			else
			{
				if (aName.Length <= 1 || aPatronymic.Length <= 1)
				{
					return false;
				}
				else
				{
					int cmp = 0;
					int len = Math.Min(aName.Length, aPatronymic.Length);
					for (int i = 1; i <= len; i++)
					{
						if (aName[i - 1] == aPatronymic[i - 1])	cmp++; else break;
					}

					return (int)cmp >= (int)Math.Round((len * 3) / 4.0);
				}
			}
		}

		public void ImportNames(TGEDCOMIndividualRecord iRec)
		{
			try
			{
				string dummy, ch_name, ch_pat;
				iRec.aux_GetNameParts(out dummy, out ch_name, out ch_pat);

				TGEDCOMSex iSex = iRec.Sex;
				this.SetNameSex(ch_name, iSex);

				if (iRec.ChildToFamilyLinks.Count != 0)
				{
					TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
					if (family != null)
					{
						TGEDCOMIndividualRecord iFather = family.Husband.Value as TGEDCOMIndividualRecord;
						if (iFather != null)
						{
							string fat_nam;
							iFather.aux_GetNameParts(out dummy, out fat_nam, out dummy);

							if (this.Comparable(fat_nam, ch_pat))
							{
								this.SetName(fat_nam, ch_pat, iSex);
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TNamesTable.ImportName(): " + E.Message);
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
						string[] data = strd.ReadLine().Trim().Split(new char[] { ';' });
						TName nm = new TName();
						nm.Name = data[0];
						nm.F_Patronymic = data[1];
						nm.M_Patronymic = data[2];
						if (data[3] != "")
						{
							nm.Sex = TGenEngine.GetSexBySign(data[3][0]);
						}
						this.FNames.Add(nm.Name, nm);
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
				foreach (DictionaryEntry de in this.FNames)
		        {
					TName nm = (de.Value as TName);
					string st = nm.Name + ";" + nm.F_Patronymic + ";" + nm.M_Patronymic + ";" + TGenEngine.SexData[(int)nm.Sex].Sign;
					strd.WriteLine(st);
		        }
			}
			finally
			{
				strd.Close();
			}
		}

		public TName AddName(string aName)
		{
			TName result = new TName();
			result.Name = aName;
			this.FNames.Add(aName, result);
			return result;
		}

		public TName FindName(string aName)
		{
			return (this.FNames[aName] as TName);
		}

		public string GetPatronymicByName(string aName, TGEDCOMSex aSex)
		{
			string result = "";

			TName nm = this.FindName(aName);
			if (nm != null)
			{
				switch (aSex) {
					case TGEDCOMSex.svMale:
						result = nm.M_Patronymic;
						break;
					case TGEDCOMSex.svFemale:
						result = nm.F_Patronymic;
						break;
				}
			}

			return result;
		}

		public string GetNameByPatronymic(string aPatronymic, TGEDCOMSex aSex)
		{
			string result = "";

			if (aPatronymic != "")
			{
				ICollection valueColl = this.FNames.Values;
				foreach (TName nm in valueColl)
				{
					if (nm.F_Patronymic == aPatronymic || nm.M_Patronymic == aPatronymic)
					{
						result = nm.Name;
						break;
					}
				}
			}

			return result;
		}

		public TGEDCOMSex GetSexByName(string aName)
		{
			TName nm = this.FindName(aName);
			return ((nm == null) ? TGEDCOMSex.svNone : nm.Sex);
		}

		public void SetName(string aName, string aPatronymic, TGEDCOMSex aSex)
		{
			if (aName != "")
			{
				TName nm = this.FindName(aName);
				if (nm == null) nm = this.AddName(aName);

				switch (aSex) {
					case TGEDCOMSex.svMale:
						if (string.IsNullOrEmpty(nm.M_Patronymic)) nm.M_Patronymic = aPatronymic;
						break;
					case TGEDCOMSex.svFemale:
						if (string.IsNullOrEmpty(nm.F_Patronymic)) nm.F_Patronymic = aPatronymic;
						break;
				}
			}
		}

		public void SetNameSex(string aName, TGEDCOMSex aSex)
		{
			if (aName != "")
			{
				TName nm = this.FindName(aName);
				if (nm == null) nm = this.AddName(aName);

				if (nm.Sex == TGEDCOMSex.svNone && aSex >= TGEDCOMSex.svMale && aSex < TGEDCOMSex.svUndetermined)
				{
					nm.Sex = aSex;
				}
			}
		}
	}
}
