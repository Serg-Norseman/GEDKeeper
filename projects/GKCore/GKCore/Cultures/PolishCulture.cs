/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih, burtek.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
