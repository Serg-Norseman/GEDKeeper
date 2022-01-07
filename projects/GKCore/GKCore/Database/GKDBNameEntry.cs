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
using GDModel;
using SQLite;

namespace GKCore.Database
{
    /// <summary>
    /// Entry of auto-updated book of names.
    /// </summary>
    [Table("NamesTable")]
    public class GKDBNameEntry
    {
        [PrimaryKey, AutoIncrement]
        public int Id { get; set; }

        [MaxLength(100), NotNull, Unique]
        public string Name { get; set; }

        [MaxLength(100)]
        public string F_Patronymic { get; set; }

        [MaxLength(100)]
        public string M_Patronymic { get; set; }

        [NotNull]
        public GDMSex Sex { get; set; }

        [Ignore]
        public object Tag { get; set; }


        public GKDBNameEntry()
        {
        }

        public GKDBNameEntry(string name) : this()
        {
            Name = name;
        }
    }
}
