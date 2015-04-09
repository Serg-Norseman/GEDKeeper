using System;
using System.Collections;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;

namespace GKCore
{
    /// <summary>
    /// Localization: dirty
    /// </summary>
    public sealed class NamesTable : BaseObject
	{
		public class NameEntry
		{
			public string Name;
			public string F_Patronymic;
			public string M_Patronymic;
			public GEDCOMSex Sex;
		}

		private readonly Hashtable fNames;

		public NamesTable()
		{
			this.fNames = new Hashtable();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				//this.FNames.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Internal functions

		private static bool Comparable(string aName, string aPatronymic)
		{
		    if (aName == null || aPatronymic == null) {
				return false;
			}
		    
            if (aName.Length <= 1 || aPatronymic.Length <= 1) {
		        return false;
		    }
		    
            int cmp = 0;
		    int len = Math.Min(aName.Length, aPatronymic.Length);
		    for (int i = 1; i <= len; i++)
		    {
		        if (aName[i - 1] == aPatronymic[i - 1])	cmp++; else break;
		    }

		    return (int)cmp >= (int)Math.Round((len * 3) / 4.0);
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
					    string line = strd.ReadLine();
                        if (string.IsNullOrEmpty(line)) continue;

						string[] data = line.Trim().Split(new char[] { ';' });
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

		public string GetPatronymicByName(string aName, GEDCOMSex aSex)
		{
			string result = "";

			NameEntry nm = this.FindName(aName);
			if (nm != null)
			{
				switch (aSex) {
					case GEDCOMSex.svMale:
						result = nm.M_Patronymic;
						break;
					case GEDCOMSex.svFemale:
						result = nm.F_Patronymic;
						break;
				}
			}

			return result;
		}

		public string GetNameByPatronymic(string aPatronymic)
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

		public GEDCOMSex GetSexByName(string aName)
		{
			NameEntry nm = this.FindName(aName);
			return ((nm == null) ? GEDCOMSex.svNone : nm.Sex);
		}

		public void SetName(string aName, string aPatronymic, GEDCOMSex aSex)
		{
			if (aName != "")
			{
				NameEntry nm = this.FindName(aName);
				if (nm == null) nm = this.AddName(aName);

				switch (aSex) {
					case GEDCOMSex.svMale:
						if (string.IsNullOrEmpty(nm.M_Patronymic)) nm.M_Patronymic = aPatronymic;
						break;
					case GEDCOMSex.svFemale:
						if (string.IsNullOrEmpty(nm.F_Patronymic)) nm.F_Patronymic = aPatronymic;
						break;
				}
			}
		}

		public void SetNameSex(string aName, GEDCOMSex aSex)
		{
			if (aName != "")
			{
				NameEntry nm = this.FindName(aName);
				if (nm == null) nm = this.AddName(aName);

				if (nm.Sex == GEDCOMSex.svNone && aSex >= GEDCOMSex.svMale && aSex < GEDCOMSex.svUndetermined)
				{
					nm.Sex = aSex;
				}
			}
		}
		
		public void ImportNames(GEDCOMIndividualRecord iRec)
		{
		    if (iRec == null) return;

			try
			{
				string dummy, ch_name, ch_pat;
				iRec.aux_GetNameParts(out dummy, out ch_name, out ch_pat);

				GEDCOMSex iSex = iRec.Sex;
				this.SetNameSex(ch_name, iSex);

				if (iRec.ChildToFamilyLinks.Count != 0)
				{
					GEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
					if (family != null)
					{
						GEDCOMIndividualRecord iFather = family.Husband.Value as GEDCOMIndividualRecord;
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
			catch (Exception ex)
			{
                SysUtils.LogWrite("NamesTable.ImportName(): " + ex.Message);
			}
		}

		#region Aux functions

		public static string ClearSurname(string surname)
		{
			if (string.IsNullOrEmpty(surname)) return "";
			
			int p = surname.IndexOf(" (");
			string result = ((p >= 0) ? surname.Substring(0, p) : surname);
			return result;
		}

		public static string PrepareRusSurname(string f, bool aFemale)
		{
			if (string.IsNullOrEmpty(f) || (f[0] == '(' && f[f.Length - 1] == ')'))
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
			if (string.IsNullOrEmpty(husbSurname)) {
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

		public static string[] GetSurnames(GEDCOMIndividualRecord iRec)
		{
			string fam, nam, pat;
			iRec.aux_GetNameParts(out fam, out nam, out pat);
			bool female = (iRec.Sex == GEDCOMSex.svFemale);

			return GetSurnames(fam, female);
		}

		public static bool StrContains(string str, char c)
		{
			return str.IndexOf(c) >= 0;
		}

		public static GEDCOMSex GetSex(string f_name, string f_pat, bool canQuery)
		{
			const string fem_endings = "ая";
			const string male_endings = "вгдйлмнопр";

			GEDCOMSex result = GEDCOMSex.svNone;
			if (string.IsNullOrEmpty(f_name)) return result;

			char nc = f_name[f_name.Length - 1];

			if (StrContains(fem_endings, nc)) {
				if (!string.IsNullOrEmpty(f_pat)) {
					char pc = f_pat[f_pat.Length - 1];

					if (StrContains(fem_endings, pc)) {
						result = GEDCOMSex.svFemale;
					} else if (StrContains(male_endings, pc)) {
						result = GEDCOMSex.svMale;
					}
				}
			} else if (StrContains(male_endings, nc)) {
				result = GEDCOMSex.svMale;
			}

			if (result == GEDCOMSex.svNone && canQuery) {
				string fn = f_name + " " + f_pat;
				DialogResult res = GKUtils.ShowQuestion("Не определяется пол человека по имени \"" + fn + "\". Это мужской пол?");
				result = (res == DialogResult.Yes) ? GEDCOMSex.svMale : GEDCOMSex.svFemale;
			}

			return result;
		}

		#endregion
	}
}
