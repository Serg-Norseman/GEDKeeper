/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

        protected DefaultCulture()
        {
        }

        public abstract bool HasPatronymic();
        public abstract bool HasSurname();

        public virtual string NormalizeSurname(string sn, bool aFemale)
        {
            return sn;
        }

        public virtual string GetMarriedSurname(string husbSurname)
        {
            return husbSurname;
        }

        public virtual GDMSex GetSex(string iName, string iPat, bool canQuery)
        {
            return GDMSex.svUnknown;
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

            var parts = GKUtils.GetNameParts(iRec);
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

        // TODO: pull the code down the hierarchy
        public NamePartsRet GetNameParts(GDMPersonalName personalName)
        {
            if (personalName == null)
                throw new ArgumentNullException("personalName");

            bool hasPatronymic = HasPatronymic();

            string stdSurname, stdName, stdPatronymic;

            // extracting standard parts
            stdSurname = personalName.Surname;

            if (hasPatronymic) {
                string firstPart = personalName.FirstPart;
                string[] parts = firstPart.Split(' ');
                if (parts.Length > 1) {
                    stdName = parts[0];
                    stdPatronymic = parts[1];
                } else {
                    stdName = firstPart;
                    stdPatronymic = string.Empty;
                }
            } else {
                stdName = personalName.FirstPart;
                stdPatronymic = string.Empty;
            }

            // extracting sub-tags parts (high priority if any)
            string surname = personalName.Pieces.Surname;
            string name = personalName.Pieces.Given;
            string patronymic = personalName.Pieces.PatronymicName;
            string marriedSurname = personalName.Pieces.MarriedName;

            if (hasPatronymic && !string.IsNullOrEmpty(name)) {
                string[] parts = name.Split(' ');
                if (string.IsNullOrEmpty(patronymic) && parts.Length > 1) {
                    name = parts[0];
                    patronymic = parts[1];
                }
            }

            surname = !string.IsNullOrEmpty(surname) ? surname : stdSurname;
            name = !string.IsNullOrEmpty(name) ? name : stdName;
            patronymic = !string.IsNullOrEmpty(patronymic) ? patronymic : stdPatronymic;

            return new NamePartsRet(surname, marriedSurname, name, patronymic);
        }
    }
}
