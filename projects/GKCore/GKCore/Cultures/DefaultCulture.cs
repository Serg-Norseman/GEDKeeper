/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using GDModel;
using GKCore.Names;

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
                throw new ArgumentNullException(nameof(iRec));

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
                throw new ArgumentNullException(nameof(personalName));

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
                throw new ArgumentNullException(nameof(iRec));

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
