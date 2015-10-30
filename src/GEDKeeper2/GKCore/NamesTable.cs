using System;
using System.Collections;
using System.IO;
using System.Text;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore
{
	/// <summary>
	/// 
	/// </summary>
	public sealed class NamesTable : BaseObject, INamesTable
	{
		private readonly Hashtable fNames;

		public NamesTable()
		{
			this.fNames = new Hashtable();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				//this.FNames.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Internal functions

		private static bool Comparable(string name, string patronymic)
		{
			if (name == null || patronymic == null) {
				return false;
			}

			if (name.Length <= 1 || patronymic.Length <= 1) {
				return false;
			}

			int cmp = 0;
			int len = Math.Min(name.Length, patronymic.Length);
			for (int i = 0; i < len; i++) {
				if (name[i] == patronymic[i])
					cmp++;
				else
					break;
			}

			return cmp >= (int)Math.Round(len * 0.5);
			// [Пав]ел/Павлович (3/5), [Ил]ья/Ильич (2/4)
		}

		#endregion

		#region Load/save functions

		public void LoadFromFile(string fileName)
		{
			if (File.Exists(fileName)) {
				StreamReader strd = new StreamReader(fileName, Encoding.GetEncoding(1251));
				try {
					while (strd.Peek() != -1) {
						string line = strd.ReadLine();
						if (string.IsNullOrEmpty(line))
							continue;

						string[] data = line.Trim().Split(new char[] { ';' });
						NameEntry nm = new NameEntry();
						nm.Name = data[0];
						nm.F_Patronymic = data[1];
						nm.M_Patronymic = data[2];
						if (data[3] != "") {
							nm.Sex = GKUtils.GetSexBySign(data[3][0]);
						}
						this.fNames.Add(nm.Name, nm);
					}
				} finally {
					strd.Close();
				}
			}
		}

		public void SaveToFile(string fileName)
		{
			StreamWriter strd = new StreamWriter(fileName, false, Encoding.GetEncoding(1251));
			try {
				foreach (DictionaryEntry de in this.fNames) {
					NameEntry nm = (de.Value as NameEntry);
					string st = nm.Name + ";" + nm.F_Patronymic + ";" + nm.M_Patronymic + ";" + GKData.SexData[(int)nm.Sex].Sign;
					strd.WriteLine(st);
				}
			} finally {
				strd.Close();
			}
		}

		#endregion

		public NameEntry AddName(string name)
		{
			NameEntry result = new NameEntry();
			result.Name = name;
			this.fNames.Add(name, result);
			return result;
		}

		public NameEntry FindName(string name)
		{
			return (this.fNames[name] as NameEntry);
		}

		public string GetPatronymicByName(string name, GEDCOMSex sex)
		{
			string result = "";

			NameEntry nm = this.FindName(name);
			if (nm != null) {
				switch (sex) {
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

		public string GetNameByPatronymic(string patronymic)
		{
			string result = "";

			if (patronymic != "") {
				foreach (NameEntry nm in this.fNames.Values) {
					if (nm.F_Patronymic == patronymic || nm.M_Patronymic == patronymic) {
						result = nm.Name;
						break;
					}
				}
			}

			return result;
		}

		public GEDCOMSex GetSexByName(string name)
		{
			NameEntry nm = this.FindName(name);
			return ((nm == null) ? GEDCOMSex.svNone : nm.Sex);
		}

		public void SetName(string name, string patronymic, GEDCOMSex sex)
		{
			if (name != "") {
				NameEntry nm = this.FindName(name);
				if (nm == null)
					nm = this.AddName(name);

				switch (sex) {
					case GEDCOMSex.svMale:
						if (string.IsNullOrEmpty(nm.M_Patronymic))
							nm.M_Patronymic = patronymic;
						break;
					case GEDCOMSex.svFemale:
						if (string.IsNullOrEmpty(nm.F_Patronymic))
							nm.F_Patronymic = patronymic;
						break;
				}
			}
		}

		public void SetNameSex(string name, GEDCOMSex sex)
		{
			if (name != "") {
				NameEntry nm = this.FindName(name);
				if (nm == null)
					nm = this.AddName(name);

				if (nm.Sex == GEDCOMSex.svNone && sex >= GEDCOMSex.svMale && sex < GEDCOMSex.svUndetermined) {
					nm.Sex = sex;
				}
			}
		}

		public void ImportNames(GEDCOMIndividualRecord iRec)
		{
			if (iRec == null)
				return;

			try {
				string dummy, ch_name, ch_pat;
				iRec.GetNameParts(out dummy, out ch_name, out ch_pat);

				GEDCOMSex iSex = iRec.Sex;
				this.SetNameSex(ch_name, iSex);

				GEDCOMIndividualRecord iFather, iMother;
				iRec.GetParents(out iFather, out iMother);

				if (iFather != null) {
					string fat_nam;
					iFather.GetNameParts(out dummy, out fat_nam, out dummy);

					if (NamesTable.Comparable(fat_nam, ch_pat)) {
						this.SetName(fat_nam, ch_pat, iSex);
					}
				}
			} catch (Exception ex) {
				SysUtils.LogWrite("NamesTable.ImportName(): " + ex.Message);
			}
		}
	}
}
