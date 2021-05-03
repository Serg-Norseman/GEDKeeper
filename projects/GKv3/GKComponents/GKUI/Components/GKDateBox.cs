/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.Globalization;
using Eto.Forms;
using GKCore;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateBox : MaskedTextBox
    {
        private readonly string fRegionalDatePattern;


        public string RegionalDatePattern
        {
            get { return fRegionalDatePattern; }
        }

        public string NormalizeDate
        {
            get { return GKUtils.GetNormalizeDate(Text, fRegionalDatePattern); }
            set { Text = GKUtils.GetRegionalDate(value, fRegionalDatePattern); }
        }


        public GKDateBox()
        {
            fRegionalDatePattern = GetShortDatePattern();
            Provider = new FixedMaskedTextProvider(GetMask(fRegionalDatePattern));
        }

        private static string GetMask(string regionalDatePattern)
        {
            // "00/00/0000"
            string result = regionalDatePattern.Replace('d', '0').Replace('m', '0').Replace('y', '0');
            return result;
        }

        private static string GetShortDatePattern()
        {
            var culture = CultureInfo.CurrentCulture; // work
            //var culture = new CultureInfo("en-US"); // debug
            //var culture = new CultureInfo("hu-HU"); // debug

            var dtf = culture.DateTimeFormat;
            var dateSeparators = dtf.DateSeparator.ToCharArray();

            // may contain a period, a dash, and a slash
            var result = dtf.ShortDatePattern.ToLowerInvariant();

            // normalize
            string[] parts = result.Split(dateSeparators, StringSplitOptions.RemoveEmptyEntries);
            for (int i = 0; i < parts.Length; i++) {
                string part = parts[i];
                char firstChar = part[0];
                switch (firstChar) {
                    case 'd':
                    case 'm':
                        if (part.Length < 2) {
                            part = part.PadRight(2, firstChar);
                        }
                        break;

                    case 'y':
                        if (part.Length < 4) {
                            part = part.PadRight(4, firstChar);
                        }
                        break;
                }
                parts[i] = part;
            }
            result = string.Join("/", parts);

            return result;
        }
    }
}
