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

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public static class GEDCOMConsts
    {
        public struct GEDCOMAppFormat
        {
            public string Sign;
            public string Name;
            
            public GEDCOMAppFormat(string sign, string name)
            {
                this.Sign = sign;
                this.Name = name;
            }
        }

        public static readonly GEDCOMAppFormat[] GEDCOMFormats;
        public static readonly string[] LngEnumStr;

        static GEDCOMConsts()
        {
            GEDCOMFormats = new GEDCOMAppFormat[] {
                new GEDCOMAppFormat("", ""),
                new GEDCOMAppFormat("GEDKeeper", ""),
                new GEDCOMAppFormat("GENBOX", "Genbox Family History"),
                new GEDCOMAppFormat("ALTREE", "Agelong Tree"),
                new GEDCOMAppFormat("AGES", "Ages!"),
                new GEDCOMAppFormat("PAF", "Personal Ancestral File"),
                new GEDCOMAppFormat("AHN", "Ahnenblatt")
            };

            LngEnumStr = new string[] {
                "", "Afrikaans", "Albanian", "Anglo-Saxon", "Catalan", "Catalan_Spn", "Czech", "Danish", "Dutch", "English",
                "Esperanto", "Estonian", "Faroese", "Finnish", "French", "German", "Hawaiian", "Hungarian", "Icelandic",
                "Indonesian", "Italian", "Latvian", "Lithuanian", "Navaho", "Norwegian", "Polish", "Portuguese", "Romanian",
                "Serbo_Croa", "Slovak", "Slovene", "Spanish", "Swedish", "Turkish", "Wendic",
                "Amharic", "Arabic", "Armenian", "Assamese", "Belorusian", "Bengali", "Braj", "Bulgarian", "Burmese",
                "Cantonese", "Church-Slavic", "Dogri", "Georgian", "Greek", "Gujarati", "Hebrew", "Hindi", "Japanese",
                "Kannada", "Khmer", "Konkani", "Korean", "Lahnda", "Lao", "Macedonian", "Maithili", "Malayalam", "Mandrin",
                "Manipuri", "Marathi", "Mewari", "Nepali", "Oriya", "Pahari", "Pali", "Panjabi", "Persian", "Prakrit", "Pusto",
                "Rajasthani", "Russian", "Sanskrit", "Serb", "Tagalog", "Tamil", "Telugu", "Thai", "Tibetan", "Ukrainian", "Urdu",
                "Vietnamese", "Yiddish" };
        }
    }
}
