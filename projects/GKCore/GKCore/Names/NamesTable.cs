/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.IO;
using System.Text;
using BSLib;
using GDModel;
using GKCore.Interfaces;

namespace GKCore.Names
{
    /// <summary>
    ///
    /// </summary>
    public sealed class NamesTable : INamesTable
    {
        private readonly Dictionary<string, NameEntry> fNames;

        public NamesTable()
        {
            fNames = new Dictionary<string, NameEntry>();
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
                fNames.Clear();

                Encoding defaultEncoding;
                try {
                    // legacy encoding
                    defaultEncoding = Encoding.GetEncoding(1251);
                } catch {
                    defaultEncoding = Encoding.UTF8;
                }

                using (var fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                    using (StreamReader reader = FileHelper.OpenStreamReader(fileStream, defaultEncoding)) {
                        while (reader.Peek() != -1) {
                            string line = reader.ReadLine();
                            if (string.IsNullOrEmpty(line)) continue;

                            string[] data = line.Trim().Split(';');
                            string nameKey = data[0];

                            if (fNames.ContainsKey(nameKey)) {
                                Logger.WriteError(string.Format("NamesTable.LoadFromFile.1(): Duplicate name in the table \"{0}\"", nameKey));
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
                }
            } catch (Exception ex) {
                Logger.WriteError("NamesTable.LoadFromFile()", ex);
            }
        }

        public void SaveToFile(string fileName)
        {
            using (var strd = new StreamWriter(fileName, false, Encoding.UTF8)) {
                foreach (KeyValuePair<string, NameEntry> de in fNames) {
                    NameEntry nm = de.Value;
                    string st = nm.Name + ";" + nm.F_Patronymic + ";" + nm.M_Patronymic + ";" + GKData.SexData[(int)nm.Sex].Sign;
                    strd.WriteLine(st);
                }
            }
        }

        #endregion

        public NameEntry AddName(string name)
        {
            var result = new NameEntry(name);
            fNames.Add(name, result);
            return result;
        }

        public NameEntry FindName(string name)
        {
            NameEntry result;
            fNames.TryGetValue(name, out result);
            return result;
        }

        public string GetPatronymicByName(string name, GDMSex sex)
        {
            string result = "";

            NameEntry nm = FindName(name);
            if (nm != null) {
                switch (sex) {
                    case GDMSex.svMale:
                        result = nm.M_Patronymic;
                        break;

                    case GDMSex.svFemale:
                        result = nm.F_Patronymic;
                        break;
                }
            }

            return result;
        }

        public string GetNameByPatronymic(string patronymic)
        {
            string result = "";

            if (!string.IsNullOrEmpty(patronymic)) {
                foreach (NameEntry nm in fNames.Values) {
                    if (nm.F_Patronymic == patronymic || nm.M_Patronymic == patronymic) {
                        result = nm.Name;
                        break;
                    }
                }
            }

            return result;
        }

        public GDMSex GetSexByName(string name)
        {
            NameEntry nm = FindName(name);
            return ((nm == null) ? GDMSex.svUnknown : nm.Sex);
        }

        public void SetName(string name, string patronymic, GDMSex sex)
        {
            if (string.IsNullOrEmpty(name)) return;

            NameEntry nm = FindName(name);
            if (nm == null) {
                nm = AddName(name);
                nm.Sex = sex;
            }

            switch (sex) {
                case GDMSex.svMale:
                    if (string.IsNullOrEmpty(nm.M_Patronymic))
                        nm.M_Patronymic = patronymic;
                    break;

                case GDMSex.svFemale:
                    if (string.IsNullOrEmpty(nm.F_Patronymic))
                        nm.F_Patronymic = patronymic;
                    break;
            }
        }

        public void SetNameSex(string name, GDMSex sex)
        {
            if (string.IsNullOrEmpty(name)) return;

            NameEntry nm = FindName(name);
            if (nm == null)
                nm = AddName(name);

            if (nm.Sex == GDMSex.svUnknown && (sex == GDMSex.svMale || sex == GDMSex.svFemale)) {
                nm.Sex = sex;
            }
        }

        public void ImportNames(IBaseContext context, GDMIndividualRecord iRec)
        {
            if (context == null || iRec == null) return;

            var persName = (iRec.PersonalNames.Count > 0) ? iRec.PersonalNames[0] : null;
            if (persName == null) return;

            ICulture culture = GKUtils.DefineCulture(context.Tree, persName);
            if (culture == null || !culture.HasPatronymic()) return;

            try {
                var parts = GKUtils.GetNameParts(context.Tree, iRec, persName, false);
                string childName = parts.Name;
                string childPat = parts.Patronymic;

                GDMSex iSex = iRec.Sex;
                SetNameSex(childName, iSex);

                GDMFamilyRecord fam = context.Tree.GetParentsFamily(iRec);
                GDMIndividualRecord father = (fam == null) ? null : context.Tree.GetPtrValue(fam.Husband);

                if (father != null) {
                    parts = GKUtils.GetNameParts(context.Tree, father, false);
                    string fatherName = parts.Name;

                    if (IsComparable(fatherName, childPat)) {
                        SetName(fatherName, childPat, iSex);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("NamesTable.ImportName()", ex);
            }
        }
    }
}
