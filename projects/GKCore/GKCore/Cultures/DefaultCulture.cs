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
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Cultures
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class DefaultCulture : ICulture
    {
        public GDMLanguageID Language { get; set; }

        public bool HasPatronymic { get; protected internal set; }
        public bool HasSurname { get; protected internal set; }

        public string SysCulture { get; protected internal set; }

        protected DefaultCulture()
        {
        }

        public virtual string NormalizeSurname(string sn, bool aFemale)
        {
            return sn;
        }

        public static string GetMaidenSurname(string surname)
        {
            if (string.IsNullOrEmpty(surname)) return string.Empty;

            int p = surname.IndexOf(" (");
            string result = ((p >= 0) ? surname.Substring(0, p) : surname);
            return result;
        }

        public virtual string GetMarriedSurname(string husbSurname)
        {
            return husbSurname;
        }

        public virtual async Task<GDMSex> GetSex(string iName, string iPat, bool canQuery)
        {
            return await Task.FromResult(GDMSex.svUnknown);
        }

        public virtual string[] GetSurnames(string surname, bool female)
        {
            string[] result = new string[1];
            result[0] = surname;
            return result;
        }

        public virtual string[] GetSurnames(GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            var parts = GetNamePartsEx(iRec);
            bool female = (iRec.Sex == GDMSex.svFemale);

            return GetSurnames(parts.Surname, female);
        }

        public virtual string GetPossessiveName(string name)
        {
            return name;
        }

        public virtual string GetPossessiveName(GDMIndividualRecord iRec)
        {
            string nm = GKUtils.GetNameString(iRec, true, false);
            nm = GetPossessiveName(nm);
            return nm;
        }

        public NamePartsRet GetNameParts(GDMPersonalName personalName)
        {
            if (personalName == null)
                throw new ArgumentNullException("personalName");

            bool hasPatronymic = HasPatronymic;

            string surname = personalName.Surname;
            string marriedSurname = personalName.MarriedName;
            string name = personalName.Given;
            string patronymic = personalName.PatronymicName;

            if (hasPatronymic && string.IsNullOrEmpty(patronymic) && !string.IsNullOrEmpty(name)) {
                string firstPart = name;
                int si = firstPart.LastIndexOf(' ');
                if (si != -1) {
                    name = firstPart.Substring(0, si);
                    patronymic = firstPart.Substring(si + 1, firstPart.Length - si - 1);
                }
            }

            return new NamePartsRet(surname, marriedSurname, name, patronymic, this);
        }

        public NamePartsRet GetNamePartsEx(GDMIndividualRecord iRec, bool formatted = true)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (iRec.PersonalNames.Count > 0) {
                GDMPersonalName personalName = iRec.PersonalNames[0];

                NamePartsRet nameParts = GetNameParts(personalName);

                if (formatted) {
                    nameParts.Surname = GKUtils.GetFmtSurname(iRec.Sex, personalName, nameParts.Surname);
                }

                return nameParts;
            } else {
                return new NamePartsRet();
            }
        }
    }
}
