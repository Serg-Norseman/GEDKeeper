/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih, burtek.
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

namespace GKCore.Cultures
{
    /// <summary>
    /// 
    /// </summary>
    public class PolishCulture : EuropeanCulture
    {
        public PolishCulture()
        {
        }

        public override string NormalizeSurname(string sn, bool aFemale)
        {
            if (string.IsNullOrEmpty(sn) || (sn[0] == '(' && sn[sn.Length - 1] == ')'))
            {
                sn = "?";
            }
            else
            {
                if (aFemale)
                {
                    sn = GetMaidenSurname(sn);

                    if (sn.EndsWith("nа")) {
                        sn = sn.Substring(0, sn.Length - 1) + "y";
                    } else if (sn.EndsWith("ska") || sn.EndsWith("cka") || sn.EndsWith("dzka")) {
                        sn = sn.Substring(0, sn.Length - 1) + "i";
                    }
                }
            }

            return sn;
        }
    }
}
