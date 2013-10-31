using System;
using System.Collections;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public sealed class NamesTable : IDisposable
	{
		public class NameEntry
		{
			public string Name;
			public string F_Patronymic;
			public string M_Patronymic;
			public TGEDCOMSex Sex;
		}

		private Hashtable fNames;
		private bool fDisposed;

		public NamesTable()
		{
			this.fNames = new Hashtable();
		}

		public void Dispose()
		{
			if (!this.fDisposed)
			{
				//this.FNames.Dispose();
				this.fDisposed = true;
			}
		}

		#region Internal functions

		private static bool Comparable(string aName, string aPatronymic)
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

		#endregion

		#region Load/save functions

		public void LoadFromFile(string aFileName)
		{
			if (File.Exists(aFileName))
			{
				StreamReader strd = new StreamReader(aFileName, Encoding.GetEncoding(1251));
				try
				{
					while (strd.Peek() != -1)
					{
						string[] data = strd.ReadLine().Trim().Split(new char[] { ';' });
						NameEntry nm = new NameEntry();
						nm.Name = data[0];
						nm.F_Patronymic = data[1];
						nm.M_Patronymic = data[2];
						if (data[3] != "")
						{
							nm.Sex = GKUtils.GetSexBySign(data[3][0]);
						}
						this.fNames.Add(nm.Name, nm);
					}
				}
				finally
				{
					strd.Close();
				}
			}
		}

		public void SaveToFile(string aFileName)
		{
			StreamWriter strd = new StreamWriter(aFileName, false, Encoding.GetEncoding(1251));
			try
			{
				foreach (DictionaryEntry de in this.fNames)
		        {
					NameEntry nm = (de.Value as NameEntry);
					string st = nm.Name + ";" + nm.F_Patronymic + ";" + nm.M_Patronymic + ";" + GKData.SexData[(int)nm.Sex].Sign;
					strd.WriteLine(st);
		        }
			}
			finally
			{
				strd.Close();
			}
		}

		#endregion

		public NameEntry AddName(string aName)
		{
			NameEntry result = new NameEntry();
			result.Name = aName;
			this.fNames.Add(aName, result);
			return result;
		}

		public NameEntry FindName(string aName)
		{
			return (this.fNames[aName] as NameEntry);
		}

		public string GetPatronymicByName(string aName, TGEDCOMSex aSex)
		{
			string result = "";

			NameEntry nm = this.FindName(aName);
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
				ICollection valueColl = this.fNames.Values;
				foreach (NameEntry nm in valueColl)
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
			NameEntry nm = this.FindName(aName);
			return ((nm == null) ? TGEDCOMSex.svNone : nm.Sex);
		}

		public void SetName(string aName, string aPatronymic, TGEDCOMSex aSex)
		{
			if (aName != "")
			{
				NameEntry nm = this.FindName(aName);
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
				NameEntry nm = this.FindName(aName);
				if (nm == null) nm = this.AddName(aName);

				if (nm.Sex == TGEDCOMSex.svNone && aSex >= TGEDCOMSex.svMale && aSex < TGEDCOMSex.svUndetermined)
				{
					nm.Sex = aSex;
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

							if (NamesTable.Comparable(fat_nam, ch_pat))
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

		#region Aux functions

		public static string ClearSurname(string surname)
		{
			int p = surname.IndexOf(" (");
			string result = ((p >= 0) ? surname.Substring(0, p) : surname);
			return result;
		}

		public static string PrepareRusSurname(string f, bool aFemale)
		{
			if (f == null || f.Length <= 0 || (f[0] == '(' && f[f.Length - 1] == ')'))
			{
				f = "?";
			}
			else
			{
				if (aFemale)
				{
					f = ClearSurname(f);

					if (f.EndsWith("а")) {
						f = f.Substring(0, f.Length - 1);
					} else if (f.EndsWith("кая")) {
						f = f.Substring(0, f.Length - 3) + "кий";
					} else if (f.EndsWith("ная")) {
						f = f.Substring(0, f.Length - 3) + "ный";
					}
				}
			}

			return f;
		}

		public static string GetRusWifeSurname(string husbSurname)
		{
			const string consonants = "бвгджзклмнпрстфхцчшщ";
			//const string vowels = "абвгдежзиклмнопрстуфхцчшщьыъэюя";
			
			string res;
			if (husbSurname == null || husbSurname.Length <= 0) {
				res = "?";
			} else {
				res = husbSurname;

				char last_sym = res[res.Length - 1];
				
				if (consonants.Contains(last_sym)) {
					res = res + "а";
				} else if (res.EndsWith("кий")) {
					res = res.Substring(0, res.Length - 3) + "кая";
				} else if (res.EndsWith("ный")) {
					res = res.Substring(0, res.Length - 3) + "ная";
				}
			}

			return res;
		}

		public static string[] GetSurnames(string surname, bool female)
		{
			string[] result = new string[1];

			if (female) {
				surname = surname.Trim();
				int p = surname.IndexOf('(');
				if (p >= 0) {
					string part = surname.Substring(0, p).Trim();
					result[0] = PrepareRusSurname(part, female);
					part = surname.Substring(p).Trim();
					part = part.Substring(1, part.Length-2);

					string[] parts = part.Split(',');
					for (int i = 0; i < parts.Length; i++) {
						string[] newres = new string[result.Length+1];
						result.CopyTo(newres, 0);
						result = newres;
						result[result.Length-1] = PrepareRusSurname(parts[i].Trim(), female);
					}
				} else {
					result[0] = PrepareRusSurname(surname, female);
				}
			} else {
				result[0] = surname;
			}

			return result;
		}

		public static string[] GetSurnames(TGEDCOMIndividualRecord iRec)
		{
			string fam, nam, pat;
			iRec.aux_GetNameParts(out fam, out nam, out pat);
			bool female = (iRec.Sex == TGEDCOMSex.svFemale);

			return GetSurnames(fam, female);
		}

		public static bool StrContains(string str, char c)
		{
			return str.IndexOf(c) >= 0;
		}

		// FIXME: localization
		public static TGEDCOMSex GetSex(string f_name, string f_pat, bool canQuery)
		{
			const string fem_endings = "ая";
			const string male_endings = "вгдйлмнопр";

			TGEDCOMSex result = TGEDCOMSex.svNone;
			if (string.IsNullOrEmpty(f_name)) return result;

			char nc = f_name[f_name.Length - 1];

			if (StrContains(fem_endings, nc)) {
				if (!string.IsNullOrEmpty(f_pat)) {
					char pc = f_pat[f_pat.Length - 1];

					if (StrContains(fem_endings, pc)) {
						result = TGEDCOMSex.svFemale;
					} else if (StrContains(male_endings, pc)) {
						result = TGEDCOMSex.svMale;
					}
				}
			} else if (StrContains(male_endings, nc)) {
				result = TGEDCOMSex.svMale;
			}

			if (result == TGEDCOMSex.svNone && canQuery) {
				string fn = f_name + " " + f_pat;
				DialogResult res = GKUtils.ShowQuestion("Не определяется пол человека по имени \"" + fn + "\". Это мужской пол?");
				result = (res == DialogResult.Yes) ? TGEDCOMSex.svMale : TGEDCOMSex.svFemale;
			}

			return result;
		}

		#endregion
	}
}
