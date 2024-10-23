/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Threading.Tasks;
using GDModel;
using GKCore.Linguistics;

namespace GKCore.Cultures
{
    /// <summary>
    /// 
    /// </summary>
    public class RussianCulture : DefaultCulture
    {
        public RussianCulture()
        {
            // default values
            HasPatronymic = true;
            HasSurname = true;
        }

        public override string NormalizeSurname(string sn, bool aFemale)
        {
            if (string.IsNullOrEmpty(sn) || (sn[0] == '(' && sn[sn.Length - 1] == ')'))
            {
                sn = "?";
            }
            else
            {
                if (aFemale)
                {
                    sn = GetMaidenSurname(sn);

                    if (sn.EndsWith("а")) {
                        sn = sn.Substring(0, sn.Length - 1);
                    } else if (sn.EndsWith("кая")) {
                        sn = sn.Substring(0, sn.Length - 3) + "кий";
                    } else if (sn.EndsWith("ная")) {
                        sn = sn.Substring(0, sn.Length - 3) + "ный";
                    }
                }
            }

            return sn;
        }

        private const string CONSONANTS = "бвгджзклмнпрстфхцчшщ";
        private const string VOWELS = "абвгдежзиклмнопрстуфхцчшщьыъэюя";

        public override string GetMarriedSurname(string husbSurname)
        {
            string res;
            if (string.IsNullOrEmpty(husbSurname)) {
                res = "?";
            } else {
                res = husbSurname;

                char lastSym = res[res.Length - 1];
                var end2 = Morpher.Right(res, 2);

                if (!"иа их ых ко ич".Contains(end2)) {
                    if (CONSONANTS.IndexOf(lastSym) >= 0) {
                        res = res + "а";
                    } else if (res.EndsWith("кий")) {
                        res = res.Substring(0, res.Length - 3) + "кая";
                    } else if (res.EndsWith("ный")) {
                        res = res.Substring(0, res.Length - 3) + "ная";
                    }
                }
            }

            return res;
        }

        private const string FEM_ENDINGS = "ая";
        private const string MALE_ENDINGS = "вгдйлмнопр";

        private static bool StrContains(string str, char c)
        {
            return str.IndexOf(c) >= 0;
        }

        public override async Task<GDMSex> GetSex(string iName, string iPat, bool canQuery)
        {
            GDMSex result = GDMSex.svUnknown;
            if (string.IsNullOrEmpty(iName)) return result;

            char nc = iName[iName.Length - 1];

            if (StrContains(FEM_ENDINGS, nc)) {
                if (!string.IsNullOrEmpty(iPat)) {
                    char pc = iPat[iPat.Length - 1];

                    if (StrContains(FEM_ENDINGS, pc)) {
                        result = GDMSex.svFemale;
                    } else if (StrContains(MALE_ENDINGS, pc)) {
                        result = GDMSex.svMale;
                    }
                }
            } else if (StrContains(MALE_ENDINGS, nc)) {
                result = GDMSex.svMale;
            }

            if (result == GDMSex.svUnknown && canQuery) {
                string fn = iName + " " + iPat;
                bool res = await AppHost.StdDialogs.ShowQuestion(string.Format(LangMan.LS(LSID.NotDeterminedPersonSex), fn));
                result = res ? GDMSex.svMale : GDMSex.svFemale;
            }

            return result;
        }

        public override string[] GetSurnames(string surname, bool female)
        {
            string[] result = new string[1];

            if (female) {
                surname = surname.Trim();
                int p = surname.IndexOf('(');
                if (p >= 0) {
                    string part = surname.Substring(0, p).Trim();
                    result[0] = NormalizeSurname(part, female);
                    part = surname.Substring(p).Trim();
                    part = part.Substring(1, part.Length-2);

                    string[] parts = part.Split(',');
                    for (int i = 0; i < parts.Length; i++) {
                        string[] newres = new string[result.Length+1];
                        result.CopyTo(newres, 0);
                        result = newres;
                        result[result.Length-1] = NormalizeSurname(parts[i].Trim(), female);
                    }
                } else {
                    result[0] = NormalizeSurname(surname, female);
                }
            } else {
                result[0] = surname;
            }

            return result;
        }

        public override string[] GetSurnames(GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            var parts = GetNamePartsEx(iRec);
            bool female = (iRec.Sex == GDMSex.svFemale);

            return GetSurnames(parts.Surname, female);
        }

        public override string GetPossessiveName(GDMIndividualRecord iRec)
        {
            var nameParts = GetNamePartsEx(iRec);
            var gender = DeclGenders[(int)iRec.Sex];

            string result = "";
            string surname = nameParts.Surname.Trim();

            int p = surname.IndexOf('(');
            if (p >= 0) {
                string part = surname.Substring(0, p).Trim();
                if (part != "") {
                    result = Morpher.GetDeclension(part, DeclensionCase.Genitive, gender, "1", 1);
                    result += " ";
                }
                part = surname.Substring(p).Trim();
                part = part.Substring(1, part.Length-2);

                result += "(";
                string[] parts = part.Split(',');
                for (int i = 0; i < parts.Length; i++) {
                    if (i > 0) result += "(";
                    part = Morpher.GetDeclension(parts[i], DeclensionCase.Genitive, gender, "1", 1);
                    result += part;
                }
                result += ")";
            } else {
                result = Morpher.GetDeclension(surname, DeclensionCase.Genitive, gender, "1", 1);
            }

            string partsMask = "";
            string nameIn = "";
            if (!string.IsNullOrEmpty(nameParts.Name)) {
                nameIn = nameParts.Name;
                partsMask = "2";
            }
            if (!string.IsNullOrEmpty(nameParts.Patronymic)) {
                if (!string.IsNullOrEmpty(nameIn)) {
                    nameIn += " ";
                }

                nameIn += nameParts.Patronymic;
                partsMask += "3";
            }

            result += " " + Morpher.GetDeclension(nameIn, DeclensionCase.Genitive, gender, partsMask, 2);
            return result;
        }

        public override string GetPossessiveName(string name)
        {
            // (genitive) "[the] sailor's / [of the] sailor"
            // (e.g. Сын моряка — художник – the sailor's son is an artist)
            return Morpher.GetDeclension(name, DeclensionCase.Genitive);
        }

        private static readonly DeclensionGender[] DeclGenders = new DeclensionGender[] {
            /* svNone */         DeclensionGender.None,
            /* svMale */         DeclensionGender.Masculine,
            /* svFemale */       DeclensionGender.Feminine,
            /* svUndetermined */ DeclensionGender.Neutral
        };
    }
}
