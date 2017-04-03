/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections;
using System.IO;
using System.Text;

using GKCommon;
using GKCommon.GEDCOM;
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
            fNames = new Hashtable();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //this.FNames.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Internal functions

        private static bool IsComparable(string name, string patronymic)
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
            // [Pav]el/Pavlovich (3/5), [Il]ja/Ilich (2/4)
        }

        #endregion

        #region Load/save functions

        public void LoadFromFile(string fileName)
        {
            if (!File.Exists(fileName)) return;

            try {
                using (StreamReader strd = new StreamReader(fileName, Encoding.UTF8)) {
                    while (strd.Peek() != -1) {
                        string line = strd.ReadLine();
                        if (string.IsNullOrEmpty(line)) continue;

                        string[] data = line.Trim().Split(';');
                        string nameKey = data[0];

                        if (fNames.ContainsKey(nameKey)) {
                            Logger.LogWrite(string.Format("NamesTable.LoadFromFile.1(): Duplicate name in the table \"{0}\"", nameKey));
                        } else {
                            NameEntry nm = new NameEntry();
                            nm.Name = nameKey;
                            nm.F_Patronymic = data[1];
                            nm.M_Patronymic = data[2];
                            if (data[3] != "") {
                                nm.Sex = GKUtils.GetSexBySign(data[3][0]);
                            }
                            fNames.Add(nameKey, nm);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("NamesTable.LoadFromFile(): " + ex.Message);
            }
        }

        public void SaveToFile(string fileName)
        {
            using (StreamWriter strd = new StreamWriter(fileName, false, Encoding.UTF8))
            {
                foreach (DictionaryEntry de in fNames)
                {
                    NameEntry nm = (NameEntry)de.Value;
                    string st = nm.Name + ";" + nm.F_Patronymic + ";" + nm.M_Patronymic + ";" + GKData.SexData[(int)nm.Sex].Sign;
                    strd.WriteLine(st);
                }
            }
        }

        #endregion

        public NameEntry AddName(string name)
        {
            NameEntry result = new NameEntry();
            result.Name = name;
            fNames.Add(name, result);
            return result;
        }

        public NameEntry FindName(string name)
        {
            return (fNames[name] as NameEntry);
        }

        public string GetPatronymicByName(string name, GEDCOMSex sex)
        {
            string result = "";

            NameEntry nm = FindName(name);
            if (nm != null)
            {
                switch (sex)
                {
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

            if (!string.IsNullOrEmpty(patronymic))
            {
                foreach (NameEntry nm in fNames.Values)
                {
                    if (nm.F_Patronymic == patronymic || nm.M_Patronymic == patronymic)
                    {
                        result = nm.Name;
                        break;
                    }
                }
            }

            return result;
        }

        public GEDCOMSex GetSexByName(string name)
        {
            NameEntry nm = FindName(name);
            return ((nm == null) ? GEDCOMSex.svNone : nm.Sex);
        }

        public void SetName(string name, string patronymic, GEDCOMSex sex)
        {
            if (string.IsNullOrEmpty(name)) return;

            NameEntry nm = FindName(name);
            if (nm == null) {
                nm = AddName(name);
                nm.Sex = sex;
            }

            switch (sex)
            {
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

        public void SetNameSex(string name, GEDCOMSex sex)
        {
            if (string.IsNullOrEmpty(name)) return;

            NameEntry nm = FindName(name);
            if (nm == null)
                nm = AddName(name);

            if (nm.Sex == GEDCOMSex.svNone && sex >= GEDCOMSex.svMale && sex < GEDCOMSex.svUndetermined)
            {
                nm.Sex = sex;
            }
        }

        public void ImportNames(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return;

            try
            {
                string dummy, childName, childPat;
                GKUtils.GetNameParts(iRec, out dummy, out childName, out childPat);

                GEDCOMSex iSex = iRec.Sex;
                SetNameSex(childName, iSex);

                GEDCOMIndividualRecord iFather, iMother;
                iRec.GetParents(out iFather, out iMother);

                if (iFather != null)
                {
                    string fatherNam;
                    GKUtils.GetNameParts(iFather, out dummy, out fatherNam, out dummy);

                    if (IsComparable(fatherNam, childPat))
                    {
                        SetName(fatherNam, childPat, iSex);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("NamesTable.ImportName(): " + ex.Message);
            }
        }
    }
}
