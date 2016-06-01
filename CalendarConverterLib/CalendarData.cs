/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2014 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.Globalization;
using System.Threading;

namespace GKCommon
{
    public static class CalendarData
    {
        public enum DateEra { AD, BC }

        public static string date_to_str(int year, int month, int day, DateEra era)
        {
            string result = string.Format("{0} {1} {2}", day.ToString(), ClassicMonths[month - 1], year.ToString());

            if (era != DateEra.AD) {
                result += " BC";
            }

            return result;
        }

        public static readonly string[] HebrewMonths;
        public static readonly string[] HebrewWeekdays;
        public static readonly string[] IslamicMonths;
        public static readonly string[] IslamicWeekdays;
        public static readonly string[] PersianMonths;
        public static readonly string[] PersianWeekdays;
        public static readonly string[] IndianCivilMonths;
        public static readonly string[] IndianCivilWeekdays;
        public static readonly string[] BahaiMonths;
        public static readonly string[] BahaiWeekdays;
        public static readonly string[] ClassicMonths;
        public static readonly string[] ClassicWeekdays;

        static CalendarData()
        {
            BahaiWeekdays = new string[] {
                "Jamál", "Kamál", "Fiḍál", "‘Idál", "Istijlál", "Istiqlál", "Jalál"
            };

            BahaiMonths = new string[] {
                "Bahá", "Jalál", "Jamál", "‘Aẓamat", "Núr", "Raḥmat", "Kalimát", "Kamál", "Asmá’", "‘Izzat",
                "Mashíyyat", "‘Ilm", "Qudrat", "Qawl", "Masá’il", "Sharaf", "Sulṭán", "Mulk", "Ayyám-i-Há", "‘Alá’"
            };


            IndianCivilWeekdays = new string[] {
                "Ravivara", "Somavara", "Mangalavara", "Budhavara", "Brahaspativara", "Sukravara", "Sanivara"
            };

            IndianCivilMonths = new string[] {
                "Caitra", "Vaisakha", "Jyaistha", "Asadha", "Sravana", "Bhadra",
                "Asvina", "Kartika", "Agrahayana", "Pausa", "Magha", "Phalguna"
            };


            PersianWeekdays = new string[] {
                "Yekshanbeh", "Doshanbeh", "Seshhanbeh", "Chaharshanbeh", "Panjshanbeh", "Jomeh", "Shanbeh"
            };

            PersianMonths = new string[] {
                "Farvardin", "Ordibehesht", "Khordad", "Tir", "Mordad", "Shahrivar", 
                "Mehr", "Aban", "Azar", "Dey", "Bahman", "Esfand",
            };


            IslamicWeekdays = new string[] {
                "Al-'ahad", "Al-'ithnayn", "Ath-thalatha'", "Al-'arb`a'", "Al-khamis", "Al-jum`a", "As-sabt"
            };

            IslamicMonths = new string[] {
                "Muharram", "Safar", "Rabi`al-Awwal", "Rabi`ath-Thani", "Jumada l-Ula", "Jumada t-Tania",
                "Rajab", "Sha`ban", "Ramadan", "Shawwal", "Dhu l-Qa`da", "Dhu l-Hijja"
            };


            HebrewWeekdays = new string[] {
                "Alef", "Bet", "Gimel", "Dalet", "He", "Vav", "Zayin"
            };

            HebrewMonths = new string[] {
                "Nisan", "Iyyar", "Sivan", "Tammuz", "Av", "Elul", 
                "Tishri", "Heshvan", "Kislev", "Teveth", "Shevat", "Adar", "Veadar"
            };


            ClassicWeekdays = new string[] {
                "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
            };

            ClassicMonths = new string[] {
                "January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"
            };
        }
    }
}
