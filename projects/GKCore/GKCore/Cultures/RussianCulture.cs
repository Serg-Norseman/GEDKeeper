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
using Externals.Linguistics;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKUI.Engine;

namespace GKCore.Cultures
{
    /// <summary>
    /// 
    /// </summary>
    public class RussianCulture : ICulture
    {
        public RussianCulture()
        {
        }

        public bool HasPatronymic()
        {
            return true;
        }

        public bool HasSurname()
        {
            return true;
        }

        private static string GetMaidenSurname(string surname)
        {
            if (string.IsNullOrEmpty(surname)) return "";

            int p = surname.IndexOf(" (");
            string result = ((p >= 0) ? surname.Substring(0, p) : surname);
            return result;
        }

        public string NormalizeSurname(string sn, bool aFemale)
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

        public string GetMarriedSurname(string husbSurname)
        {
            const string consonants = "бвгджзклмнпрстфхцчшщ";
            //const string vowels = "абвгдежзиклмнопрстуфхцчшщьыъэюя";

            string res;
            if (string.IsNullOrEmpty(husbSurname)) {
                res = "?";
            } else {
                res = husbSurname;

                char lastSym = res[res.Length - 1];

                if (consonants.IndexOf(lastSym) >= 0) {
                    res = res + "а";
                } else if (res.EndsWith("кий")) {
                    res = res.Substring(0, res.Length - 3) + "кая";
                } else if (res.EndsWith("ный")) {
                    res = res.Substring(0, res.Length - 3) + "ная";
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

        public GEDCOMSex GetSex(string iName, string iPat, bool canQuery)
        {
            GEDCOMSex result = GEDCOMSex.svNone;
            if (string.IsNullOrEmpty(iName)) return result;

            char nc = iName[iName.Length - 1];

            if (StrContains(FEM_ENDINGS, nc)) {
                if (!string.IsNullOrEmpty(iPat)) {
                    char pc = iPat[iPat.Length - 1];

                    if (StrContains(FEM_ENDINGS, pc)) {
                        result = GEDCOMSex.svFemale;
                    } else if (StrContains(MALE_ENDINGS, pc)) {
                        result = GEDCOMSex.svMale;
                    }
                }
            } else if (StrContains(MALE_ENDINGS, nc)) {
                result = GEDCOMSex.svMale;
            }

            if (result == GEDCOMSex.svNone && canQuery) {
                string fn = iName + " " + iPat;
                bool res = AppHub.StdDialogs.ShowQuestionYN(string.Format(LangMan.LS(LSID.LSID_NotDeterminedPersonSex), fn));
                result = (res == true) ? GEDCOMSex.svMale : GEDCOMSex.svFemale;
            }

            return result;
        }

        public string[] GetSurnames(string surname, bool female)
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

        public string[] GetSurnames(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            string fam, nam, pat;
            GKUtils.GetNameParts(iRec, out fam, out nam, out pat);
            bool female = (iRec.Sex == GEDCOMSex.svFemale);

            return GetSurnames(fam, female);
        }

        public string GetPossessiveName(string name)
        {
            // (genitive) "[the] sailor's / [of the] sailor"
            // (e.g. Сын моряка — художник – the sailor's son is an artist)
            return RusDeclension.GetDeclension(name, DeclensionCase.Genitive);
        }
    }
}
