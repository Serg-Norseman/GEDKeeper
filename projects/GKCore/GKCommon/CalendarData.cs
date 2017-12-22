/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2014 by Sergey V. Zhdanovskih.
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

namespace GKCommon
{
    public static class CalendarData
    {
        // Default names
        public static readonly string[] BahaiMonths;
        public static readonly string[] BahaiWeekdays;
        public static readonly string[] ClassicMonths;
        public static readonly string[] ClassicWeekdays;
        public static readonly string[] HebrewMonths;
        public static readonly string[] HebrewWeekdays;
        public static readonly string[] IndianCivilMonths;
        public static readonly string[] IndianCivilWeekdays;
        public static readonly string[] IslamicMonths;
        public static readonly string[] IslamicWeekdays;
        public static readonly string[] PersianMonths;
        public static readonly string[] PersianWeekdays;

        public static string[] InitNames(string text)
        {
            return text.Split('|');
        }

        static CalendarData()
        {
            BahaiMonths = InitNames("Bahá|Jalál|Jamál|‘Aẓamat|Núr|Raḥmat|Kalimát|Kamál|Asmá’|‘Izzat|"+
                                    "Mashíyyat|‘Ilm|Qudrat|Qawl|Masá’il|Sharaf|Sulṭán|Mulk|Ayyám-i-Há|‘Alá’");
            BahaiWeekdays = InitNames("Jamál|Kamál|Fiḍál|‘Idál|Istijlál|Istiqlál|Jalál");


            ClassicMonths = InitNames("January|February|March|April|May|June|July|August|September|October|November|December");
            ClassicWeekdays = InitNames("Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday");


            HebrewMonths = InitNames("Nisan|Iyyar|Sivan|Tammuz|Av|Elul|Tishri|Heshvan|Kislev|Teveth|Shevat|Adar|Veadar");
            HebrewWeekdays = InitNames("Alef|Bet|Gimel|Dalet|He|Vav|Zayin");


            IndianCivilMonths = InitNames("Caitra|Vaisakha|Jyaistha|Asadha|Sravana|Bhadra|"+
                                          "Asvina|Kartika|Agrahayana|Pausa|Magha|Phalguna");
            IndianCivilWeekdays = InitNames("Ravivara|Somavara|Mangalavara|Budhavara|Brahaspativara|Sukravara|Sanivara");


            IslamicMonths = InitNames("Muharram|Safar|Rabi`al-Awwal|Rabi`ath-Thani|Jumada l-Ula|Jumada t-Tania|"+
                                      "Rajab|Sha`ban|Ramadan|Shawwal|Dhu l-Qa`da|Dhu l-Hijja");
            IslamicWeekdays = InitNames("Al-'ahad|Al-'ithnayn|Ath-thalatha'|Al-'arb`a'|Al-khamis|Al-jum`a|As-sabt");


            PersianMonths = InitNames("Farvardin|Ordibehesht|Khordad|Tir|Mordad|Shahrivar|Mehr|Aban|Azar|Dey|Bahman|Esfand");
            PersianWeekdays = InitNames("Yekshanbeh|Doshanbeh|Seshhanbeh|Chaharshanbeh|Panjshanbeh|Jomeh|Shanbeh");
        }
    }
}
