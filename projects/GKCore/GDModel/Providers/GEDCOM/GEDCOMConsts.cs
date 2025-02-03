/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

namespace GDModel.Providers.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public static class GEDCOMConsts
    {
        public static readonly string[] GEDCOMDateTypes;

        public const char Delimiter = ' ';
        public const char YearModifierSeparator = '/';
        public const char NameSeparator = '/';
        public const string YearBC = "B.C.";
        public const char PointerDelimiter = '@';
        public const string NewLine = "\r\n";
        public const int MaxLineLength = 248;
        public const int MaxNoteSize = 32768;

        // deprecated
        //public const byte GEDCOMMaxPhoneNumbers = 3;
        //public const byte GEDCOMMaxEmailAddresses = 3;
        //public const byte GEDCOMMaxFaxNumbers = 3;
        //public const byte GEDCOMMaxWebPages = 3;
        //public const byte GEDCOMMaxLanguages = 3;

        internal static readonly string[] GEDCOMDateApproximatedArray;
        internal static readonly string[] GEDCOMDateRangeArray;
        internal static readonly string[] GEDCOMDateEscapeArray;

        public static readonly string[] GEDCOMMonthArray;
        internal static readonly string[] GEDCOMMonthHebrewArray;
        internal static readonly string[] GEDCOMMonthIslamicArray;
        internal static readonly string[] GEDCOMMonthFrenchArray;
        internal static readonly string[] GEDCOMMonthSysArray;

        internal static readonly EnumTuple[] GEDCOMMonthValues;

        static GEDCOMConsts()
        {
            GEDCOMDateTypes = new string[] { "", "ABT", "AFT", "BEF", "BET", "CAL", "EST", "FROM", "INT", "TO" };

            GEDCOMDateApproximatedArray = new string[] { "", "ABT", "CAL", "EST" };
            GEDCOMDateRangeArray = new string[] { "AFT", "BEF", "BET", "AND" };

            GEDCOMDateEscapeArray = new string[]
            {
                "@#DGREGORIAN@", "@#DJULIAN@", "@#DHEBREW@", "@#DFRENCH R@", "@#DROMAN@",
                "@#DISLAMIC@", // GK+ (nonstandard)
                "@#DUNKNOWN@"
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

            // TODO: not implemented yet
            GEDCOMMonthIslamicArray = new string[]
            {
                "", "", "", "", "", "",
                "", "", "", "", "", ""
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

            // post-arranged array
            GEDCOMMonthValues = new EnumTuple[] {
                new EnumTuple("JAN", 1), // J/G
                new EnumTuple("FEB", 2), // J/G
                new EnumTuple("MAR", 3), // J/G
                new EnumTuple("APR", 4), // J/G
                new EnumTuple("MAY", 5), // J/G
                new EnumTuple("JUN", 6), // J/G
                new EnumTuple("JUL", 7), // J/G
                new EnumTuple("AUG", 8), // J/G
                new EnumTuple("SEP", 9), // J/G
                new EnumTuple("OCT", 10), // J/G
                new EnumTuple("NOV", 11), // J/G
                new EnumTuple("DEC", 12), // J/G

                new EnumTuple("TSH", 1), // H
                new EnumTuple("CSH", 2), // H
                new EnumTuple("KSL", 3), // H
                new EnumTuple("TVT", 4), // H
                new EnumTuple("SHV", 5), // H
                new EnumTuple("ADR", 6), // H
                new EnumTuple("ADS", 7), // H
                new EnumTuple("NSN", 8), // H
                new EnumTuple("IYR", 9), // H
                new EnumTuple("SVN", 10), // H
                new EnumTuple("TMZ", 11), // H
                new EnumTuple("AAV", 12), // H
                new EnumTuple("ELL", 13), // H

                new EnumTuple("VEND", 1), // F
                new EnumTuple("BRUM", 2), // F
                new EnumTuple("FRIM", 3), // F
                new EnumTuple("NIVO", 4), // F
                new EnumTuple("PLUV", 5), // F
                new EnumTuple("VENT", 6), // F
                new EnumTuple("GERM", 7), // F
                new EnumTuple("FLOR", 8), // F
                new EnumTuple("PRAI", 9), // F
                new EnumTuple("MESS", 10), // F
                new EnumTuple("THER", 11), // F
                new EnumTuple("FRUC", 12), // F
                new EnumTuple("COMP", 13), // F

                // for files with poor standard support (Russian-localized names of the months)
                new EnumTuple("ЯНВ", 1), // J/G
                new EnumTuple("ФЕВ", 2), // J/G
                new EnumTuple("МАР", 3), // J/G
                new EnumTuple("АПР", 4), // J/G
                new EnumTuple("МАЙ", 5), // J/G
                new EnumTuple("ИЮН", 6), // J/G
                new EnumTuple("ИЮЛ", 7), // J/G
                new EnumTuple("АВГ", 8), // J/G
                new EnumTuple("СЕН", 9), // J/G
                new EnumTuple("ОКТ", 10), // J/G
                new EnumTuple("НОЯ", 11), // J/G
                new EnumTuple("ДЕК", 12), // J/G

                // for files with poor standard support (Dutch/French/German/Spanish-localized names of the months)
                new EnumTuple("MRT", 3), // Dutch
                new EnumTuple("MEI", 5), // Dutch
                new EnumTuple("OKT", 10), // Dutch/German

                new EnumTuple("FÉV", 2), // French
                new EnumTuple("AVR", 4), // French
                new EnumTuple("MAI", 5), // French/German
                new EnumTuple("AOÛ", 8), // French
                new EnumTuple("DÉC", 12), // French

                new EnumTuple("MÄR", 3), // German
                new EnumTuple("MRZ", 3), // German
                new EnumTuple("DEZ", 12), // German

                new EnumTuple("ENE", 1), // Spanish
                new EnumTuple("ABR", 4), // Spanish
                new EnumTuple("AGO", 8), // Spanish
                new EnumTuple("DIC", 12), // Spanish
            };
            // BinarySearch requires a sorted array
            Array.Sort(GEDCOMMonthValues);
        }
    }
}
