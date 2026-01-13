/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;

namespace GKCore.Names
{
    /// <summary>
    /// Entry of auto-updated book of names.
    /// </summary>
    public class NameEntry
    {
        public string Name;
        public string F_Patronymic;
        public string M_Patronymic;
        public GDMSex Sex;

        public NameEntry()
        {
        }

        public NameEntry(string name) : this()
        {
            Name = name;
        }
    }
}
