/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

namespace GKCommon.GEDCOM
{
    public abstract class GEDCOMCustomDate : GEDCOMTag, IComparable
    {
        public static readonly string[] GEDCOMDateApproximatedArray;
        public static readonly string[] GEDCOMDateRangeArray;
        public static readonly string[] GEDCOMDateEscapeArray;

        public static readonly string[] GEDCOMMonthArray;
        public static readonly string[] GEDCOMMonthHebrewArray;
        public static readonly string[] GEDCOMMonthFrenchArray;

        public static readonly string[] GEDCOMMonthSysArray;
        public static readonly string[] GEDCOMMonthRusArray;

        static GEDCOMCustomDate()
        {
            GEDCOMDateApproximatedArray = new string[] { "", "ABT", "CAL", "EST" };
            GEDCOMDateRangeArray = new string[] { "AFT", "BEF", "BET", "AND" };

            GEDCOMDateEscapeArray = new string[]
            {
                "@#DGREGORIAN@", "@#DJULIAN@", "@#DHEBREW@", "@#DFRENCH R@", "@#DROMAN@", "@#DUNKNOWN@"
            };

            GEDCOMMonthArray = new string[]
            {
                "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
            };

            GEDCOMMonthHebrewArray = new string[]
            {
                "TSH", "CSH", "KSL", "TVT", "SHV", "ADR",
                "ADS", "NSN", "IYR", "SVN", "TMZ", "AAV", "ELL"
            };

            GEDCOMMonthFrenchArray = new string[]
            {
                "VEND", "BRUM", "FRIM", "NIVO", "PLUV", "VENT",
                "GERM", "FLOR", "PRAI", "MESS", "THER", "FRUC", "COMP"
            };

            GEDCOMMonthSysArray = new string[]
            {
                "01.", "02.", "03.", "04.", "05.", "06.",
                "07.", "08.", "09.", "10.", "11.", "12."
            };

            GEDCOMMonthRusArray = new string[]
            {
                "ﬂÕ¬", "‘≈¬", "Ã¿–", "¿œ–", "Ã¿…", "»ﬁÕ",
                "»ﬁÀ", "¿¬√", "—≈Õ", "Œ “", "ÕŒﬂ", "ƒ≈ "
            };
        }

        public DateTime Date
        {
            get { return this.GetDateTime(); }
            set { this.SetDateTime(value); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.SetName("DATE");
        }

        protected GEDCOMCustomDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public abstract DateTime GetDateTime();
        public abstract void SetDateTime(DateTime value);
        public abstract void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC);

        /**
         * Obtaining UDN (Unified Date Number) for purposes of processing and sorting.
         */
        public abstract UDN GetUDN();

        public int CompareTo(object obj)
        {
            GEDCOMCustomDate otherDate = obj as GEDCOMCustomDate;

            if (otherDate != null) {
                UDN abs1 = this.GetUDN();
                UDN abs2 = otherDate.GetUDN();
                return abs1.CompareTo(abs2);
            }

            return -1;
        }
    }
}
