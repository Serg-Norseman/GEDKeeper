/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#if !TERM

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

#endif
