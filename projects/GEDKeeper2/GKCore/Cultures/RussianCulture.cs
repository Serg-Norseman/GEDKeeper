/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;
using GKCommon.GEDCOM;

namespace GKCore.Cultures
{
    /// <summary>
    /// 
    /// </summary>
    public class RussianCulture
    {
        public RussianCulture()
        {
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
                    f = GKUtils.ClearSurname(f);

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

        public static GEDCOMSex GetSex(string iName, string iPat, bool canQuery)
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
                DialogResult res = GKUtils.ShowQuestion(string.Format(LangMan.LS(LSID.LSID_NotDeterminedPersonSex), fn));
                result = (res == DialogResult.Yes) ? GEDCOMSex.svMale : GEDCOMSex.svFemale;
            }

            return result;
        }
    }
}
