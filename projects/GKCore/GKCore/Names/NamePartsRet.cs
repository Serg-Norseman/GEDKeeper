/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Cultures;

namespace GKCore.Names
{
    public sealed class NamePartsRet
    {
        public static readonly NamePartsRet Empty = new NamePartsRet();


        // Simple or maiden surname
        public string Surname;

        public string Name;
        public string Patronymic;
        public ICulture Culture;

        public string MarriedSurname;


        public NamePartsRet()
        {
            Surname = string.Empty;
            Name = string.Empty;
            Patronymic = string.Empty;
            MarriedSurname = string.Empty;
        }

        public NamePartsRet(string surname, string name)
        {
            Surname = surname;
            Name = name;

            Patronymic = string.Empty;
            MarriedSurname = string.Empty;
        }

        public NamePartsRet(string surname, string name, string patronymic)
        {
            Surname = surname;
            Name = name;
            Patronymic = patronymic;

            MarriedSurname = string.Empty;
        }

        public NamePartsRet(string maidenSurname, string marriedSurname, string name, string patronymic, ICulture culture)
        {
            Surname = maidenSurname;
            MarriedSurname = marriedSurname;
            Name = name;
            Patronymic = patronymic;
            Culture = culture;
        }
    }
}
