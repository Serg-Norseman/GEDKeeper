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
