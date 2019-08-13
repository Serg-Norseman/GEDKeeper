/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections;
using System.Globalization;

namespace GEDmill.Model
{
    /// <summary>
    /// Used to sort the individuals index.
    /// </summary>
    public class AlphabetComparer : IComparer
    {
        // Part of the comparison is delegated to a dotnet slc, declared here
        private CaseInsensitiveComparer fComparer;

        // Define the alphabetical ordering, including non-English characters
        private const string Alphabet = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ∆ÿ≈ƒ÷[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";


        public AlphabetComparer()
        {
            fComparer = new CaseInsensitiveComparer(CultureInfo.CurrentCulture);
        }

        // The comparison function - the raison d'etre of this class
        public int Compare(object x, object y)
        {
            string sx = x.ToString();
            string sy = y.ToString();

            char[] acx = sx.ToCharArray();
            char[] acy = sy.ToCharArray();

            int l = sx.Length;
            if (sy.Length < l) {
                l = sy.Length;
            }

            // Ensure No Surname people go at top of list
            if (acx[0] == ',' && acy[0] != ',') {
                return -1;
            }
            if (acx[0] != ',' && acy[0] == ',') {
                return 1;
            }

            for (int i = 0; i < l; ++i) {
                int ix = Alphabet.IndexOf(Char.ToUpper(acx[i]));
                int iy = Alphabet.IndexOf(Char.ToUpper(acy[i]));
                if (iy == -1 || ix == -1) {
                    return fComparer.Compare(x, y);
                }
                if (ix < iy) {
                    return -1;
                }
                if (ix > iy) {
                    return 1;
                }
            }

            if (sx.Length == sy.Length) {
                return 0;
            }
            if (sx.Length < sy.Length) {
                return -1;
            }
            return 1;
        }
    }
}
