/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Cultures
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class IcelandCulture : DefaultCulture
    {
        public IcelandCulture()
        {
            // default values
            HasPatronymic = true;
            HasSurname = false; // doesn't exist and is prohibited by law from July 27, 1925
        }
    }
}
