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

using System.Globalization;
using System.Threading;

namespace GKCommon
{
    public static class CalendarData
    {
        public enum DateEra
        {
            AD,
            BC
        }

        public static string date_to_str(int year, int month, int day, DateEra era)
        {
            DateTimeFormatInfo dtInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;

            string result = string.Concat(new string[] {
                                              day.ToString(),  " ",  dtInfo.AbbreviatedMonthNames[month - 1],  " ",  year.ToString()
                                          });

            if (era != DateEra.AD) {
                result += " до н.э.";
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

        static CalendarData()
        {
            BahaiWeekdays = new string[]
            {
                "Джамаль",
                "Камаль",
                "Фидаль",
                "Идаль",
                "Истиджлаль",
                "Истиклаль",
                "Джалаль"
            };

            BahaiMonths = new string[]
            {
                "Бахa",
                "Джалaл",
                "Джамaл",
                "Азамат",
                "Нур",
                "Рахмат",
                "Калимaт",
                "Камaл",
                "Асмa",
                "Иззат",
                "Машиййат",
                "Ильм",
                "Кудрат",
                "Каул",
                "Масa’иль",
                "Шараф",
                "Султан",
                "Мульк",
                "Аййaм-и Хa",
                "Алa"
            };

            IndianCivilWeekdays = new string[]
            {
                "равивар",
                "сомвар",
                "мангалвар",
                "будхвар",
                "брихаспативар",
                "шукрвар",
                "шанивар"
            };

            IndianCivilMonths = new string[]
            {
                "Чайтра",
                "Ваисакха",
                "Джанштха",
                "Асадха",
                "Сравана",
                "Бхадра",
                "Азвина",
                "Картика",
                "Аграхайана",
                "Пауза",
                "Магха",
                "Пхалгуна"
            };

            PersianWeekdays = new string[]
            {
                "йекшанбе",
                "душанбе",
                "сешанбе",
                "чахаршанбе",
                "панджшанбе",
                "джоме",
                "шанбе"
            };

            PersianMonths = new string[]
            {
                "Фарвардин",
                "Ордибехешт",
                "Хордад",
                "Тир",
                "Мордад",
                "Шахривар",
                "Мехр",
                "Абан",
                "Азар",
                "Дей",
                "Бахман",
                "Эсфанд"
            };

            IslamicWeekdays = new string[]
            {
                "аль-ахад",
                "аль-иснайн",
                "ас-саласа'",
                "аль-арба'а",
                "аль-хамис",
                "аль-джум'а",
                "ас-сабт"
            };

            IslamicMonths = new string[]
            {
                "мухаррам",
                "сафар",
                "рабии`у ль-авваль",
                "рабии`у с-саании",
                "джумаада ль-ууля",
                "джумаада ль-аахыр",
                "раджаб",
                "шаабан",
                "рамадан",
                "шавваль",
                "зуль-ка`да",
                "зульхиджа"
            };

            HebrewWeekdays = new string[]
            {
                "алеф",
                "бейт",
                "гимел",
                "далет",
                "хей",
                "вав",
                "зайин"
            };

            HebrewMonths = new string[]
            {
                "Нисан",
                "Ияр",
                "Сиван",
                "Тамуз",
                "Ав",
                "Элул",
                "Тишрей",
                "Хешван",
                "Кислев",
                "Тевет",
                "Шват",
                "Адар",
                "Адар бет"
            };
        }
    }
}
