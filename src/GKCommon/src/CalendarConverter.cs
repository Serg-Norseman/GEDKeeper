/*
 *  CalendarConverter, part of project "Fourmilab Calendar Converter"
 *  Partial copyright by John Walker
 *  http://www.fourmilab.ch/documents/calendar/
 *
 *  C# implementation:
 *  Copyright (C) 2011 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 */

using System;
using System.Globalization;
using System.Threading;
using GKCommon;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCommon
{
	public static class CalendarConverter
	{
		public enum TDateEra : byte
		{
			AD,
			BC
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

		public static int iFloor(double X)
		{
			return (int)Math.Floor(X);
		}

		public static int iCeil(double X)
		{
			return (int)Math.Ceiling(X);
		}

		private static double _modf(double a, double b)
		{
			return (a - b * Math.Floor((a / b)));
		}

		private static int _modi(double a, double b)
		{
			return (int)SysUtils.Trunc(a - b * Math.Floor((a / b)));
		}

		private static int _if(bool aCond, int aThen, int aElse)
		{
			int result = aCond ? aThen : aElse;
			return result;
		}

		public static bool hebrew_leap(int year)
		{
			return _modf((double)(year * 7 + 1), 19.0) < (double)7f;
		}

		private static int hebrew_year_months(int year)
		{
			return _if(hebrew_leap(year), 13, 12);
		}

		private static int hebrew_delay_1(int year)
		{
			int months = iFloor(((double)(235 * year - 234) / 19.0));
			int parts = 12084 + 13753 * months;
			int day = months * 29 + iFloor(((double)parts / 25920.0));
			if (_modf((double)(3 * (day + 1)), 7.0) < (double)3f)
			{
				day++;
			}
			return day;
		}

		private static int hebrew_delay_2(int year)
		{
			int last = hebrew_delay_1(year - 1);
			int present = hebrew_delay_1(year);
			int next = hebrew_delay_1(year + 1);
			return _if(next - present == 356, 2, _if(present - last == 382, 1, 0));
		}

		private static double hebrew_year_days(int year)
		{
			return (hebrew_to_jd(year + 1, 7, 1) - hebrew_to_jd(year, 7, 1));
		}

		private static int hebrew_month_days(int year, int month)
		{
			int Result;
			if (month == 2 || month == 4 || month == 6 || month == 10 || month == 13)
			{
				Result = 29;
			}
			else
			{
				if (month == 12 && !hebrew_leap(year))
				{
					Result = 29;
				}
				else
				{
					if (month == 8 && _modf(hebrew_year_days(year), 10.0) != (double)5f)
					{
						Result = 29;
					}
					else
					{
						if (month == 9 && _modf(hebrew_year_days(year), 10.0) == (double)3f)
						{
							Result = 29;
						}
						else
						{
							Result = 30;
						}
					}
				}
			}
			return Result;
		}

		public static bool leap_gregorian(int year)
		{
			return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
		}

		public static bool leap_julian(int year)
		{
			return _modf((double)year, 4.0) == (double)_if(year > 0, 0, 3);
		}

		public static bool leap_islamic(int year)
		{
			return (year * 11 + 14) % 30 < 11;
		}

		public static bool leap_persian(int year)
		{
			return ((year - _if(year > 0, 474, 473)) % 2820 + 474 + 38) * 682 % 2816 < 682;
		}

		public static int jwday(double j)
		{
			return _modi(Math.Floor((j + 1.5)), 7.0);
		}

		public static double gregorian_to_jd(int year, int month, int day)
		{
			return (1721424.5 + (double)(365 * (year - 1)) + Math.Floor(((double)(year - 1) / 4.0)) + -Math.Floor(((double)(year - 1) / 100.0)) + Math.Floor(((double)(year - 1) / 400.0)) + Math.Floor(((double)(367 * month - 362) / 12.0 + (double)_if(month <= 2, 0, _if(leap_gregorian(year), -1, -2)) + (double)day)));
		}

        public static void jd_to_gregorian(double jd, out int year, out int month, out int day)
		{
			double wjd = (Math.Floor((jd - 0.5)) + 0.5);
			double depoch = (wjd - 1721425.5);
			int quadricent = iFloor((depoch / 146097.0));
			double dqc = _modf(depoch, 146097.0);
			int cent = iFloor((dqc / 36524.0));
			double dcent = _modf(dqc, 36524.0);
			int quad = iFloor((dcent / 1461.0));
			double dquad = _modf(dcent, 1461.0);
			int yindex = iFloor((dquad / 365.0));
			year = quadricent * 400 + cent * 100 + (quad << 2) + yindex;
			if (cent != 4 && yindex != 4)
			{
				year++;
			}
			double yearday = (wjd - gregorian_to_jd(year, 1, 1));
			int leapadj = _if(wjd < gregorian_to_jd(year, 3, 1), 0, _if(leap_gregorian(year), 1, 2));
			month = iFloor((((yearday + (double)leapadj) * 12.0 + 373.0) / 367.0));
			day = (int)(Math.Truncate(wjd - gregorian_to_jd(year, month, 1)) + 1);
		}

		public static double julian_to_jd(int year, int month, int day)
		{
			if (year < 1)
			{
				year++;
			}
			if (month <= 2)
			{
				year--;
				month += 12;
			}
			return (Math.Floor((365.25 * (double)(year + 4716))) + Math.Floor((30.6001 * (double)(month + 1))) + (double)day - 1524.5);
		}

		public static void jd_to_julian(double jd, ref int year, ref int month, ref int day)
		{
			jd = (jd + 0.5);
			int b = iFloor(jd) + 1524;
			int c = iFloor((((double)b - 122.1) / 365.25));
			int d = iFloor((365.25 * (double)c));
			int e = iFloor(((double)(b - d) / 30.6001));
			month = iFloor((double)_if(e < 14, e - 1, e - 13));
			year = iFloor((double)_if(month > 2, c - 4716, c - 4715));
			day = b - d - iFloor((30.6001 * (double)e));
			if (year < 1)
			{
				year--;
			}
		}

		public static double hebrew_to_jd(int year, int month, int day)
		{
			int months = hebrew_year_months(year);
			double jd = (347995.5 + (double)hebrew_delay_1(year) + (double)hebrew_delay_2(year) + (double)day + 1.0);
			if (month < 7)
			{
				int num = months;
				int mon = 7;
				if (num >= mon)
				{
					num++;
					do
					{
						jd = (jd + (double)hebrew_month_days(year, mon));
						mon++;
					}
					while (mon != num);
				}

				int num2 = month - 1;
				mon = 1;
				if (num2 >= mon)
				{
					num2++;
					do
					{
						jd = (jd + (double)hebrew_month_days(year, mon));
						mon++;
					}
					while (mon != num2);
				}
			}
			else
			{
				int num3 = month - 1;
				int mon = 7;
				if (num3 >= mon)
				{
					num3++;
					do
					{
						jd = (jd + (double)hebrew_month_days(year, mon));
						mon++;
					}
					while (mon != num3);
				}
			}
			return jd;
		}

		public static void jd_to_hebrew(double jd, ref int year, ref int month, ref int day)
		{
			jd = (Math.Floor(jd) + 0.5);
			int count = iFloor(((jd - 347995.5) * 98496.0 / 35975351.0));
			year = count - 1;
			int i = count;
			while (jd >= hebrew_to_jd(i, 7, 1))
			{
				i++;
				year++;
			}
			int first = _if(jd < hebrew_to_jd(year, 1, 1), 7, 1);
			month = first;
			i = first;
			while (jd > hebrew_to_jd(year, i, hebrew_month_days(year, i)))
			{
				i++;
				month++;
			}
			day = (int)Math.Truncate(jd - hebrew_to_jd(year, month, 1) + 1.0);
		}

		public static double islamic_to_jd(int year, int month, int day)
		{
			return ((double)(day + Math.Ceiling((29.5 * (double)(month - 1))) + (year - 1) * 354 + Math.Floor(((double)(3 + 11 * year) / 30.0))) + 1948439.5 - 1.0);
		}

        public static void jd_to_islamic(double jd, ref int year, ref int month, ref int day)
		{
			jd = ((double)Math.Floor(jd) + 0.5);
			year = iFloor(((30.0 * (jd - 1948439.5) + 10646.0) / 10631.0));
			month = Math.Min(12, iCeil(((jd - (29.0 + islamic_to_jd(year, 1, 1))) / 29.5)) + 1);
			day = (int)Math.Truncate(jd - islamic_to_jd(year, month, 1) + 1.0);
		}

		public static double persian_to_jd(int year, int month, int day)
		{
			double epbase = (double)(year - _if(year >= 0, 474, 473));
			double epyear = (474.0 + _modf(epbase, 2820.0));
			return ((double)(day + _if(month <= 7, (month - 1) * 31, (month - 1) * 30 + 6) + Math.Floor(((epyear * 682.0 - 110.0) / 2816.0))) + (epyear - 1.0) * 365.0 + (Math.Floor((epbase / 2820.0)) * 1029983) + 1948319.5);
		}

		public static void jd_to_persian(double jd, ref int year, ref int month, ref int day)
		{
			jd = (Math.Floor(jd) + 0.5);
			double depoch = (jd - persian_to_jd(475, 1, 1));
			int cycle = iFloor((depoch / 1029983.0));
			int cyear = _modi(depoch, 1029983.0);
			int ycycle;
			if (cyear == 1029982)
			{
				ycycle = 2820;
			}
			else
			{
				int aux = iFloor(((double)cyear / 366.0));
				int aux2 = _modi((double)cyear, 366.0);
				ycycle = iFloor(((double)(2134 * aux + 2816 * aux2 + 2815) / 1028522.0)) + aux + 1;
			}
			year = ycycle + 2820 * cycle + 474;
			if (year <= 0)
			{
				year--;
			}
			double yday = (jd - persian_to_jd(year, 1, 1) + 1.0);
			month = _if(yday <= (double)186f, iCeil((yday / 31.0)), iCeil(((yday - 6.0) / 30.0)));
			day = (int)Math.Truncate(jd - persian_to_jd(year, month, 1) + 1.0);
		}

		public static double indian_civil_to_jd(int year, int month, int day)
		{
			int gyear = year + 78;
			bool leap = leap_gregorian(gyear);
			double start = gregorian_to_jd(gyear, 3, _if(leap, 21, 22));
			int Caitra = _if(leap, 31, 30);
			double jd;
			if (month == 1)
			{
				jd = (start + (double)(day - 1));
			}
			else
			{
				jd = (start + (double)Caitra);
				int i = month - 2;
				i = Math.Min(i, 5);
				jd = (jd + (double)(i * 31));
				if (month >= 8)
				{
					i = month - 7;
					jd = (jd + (double)(i * 30));
				}
				jd = (jd + (double)day - 1.0);
			}
			return jd;
		}

		public static void jd_to_indian_civil(double jd, ref int year, ref int month, ref int day)
		{
			int Saka = 78;
			double start = 80.0;
			jd = (Math.Floor(jd) + 0.5);
			int greg_y = 0;
			int greg_m = 0;
			int greg_d = 0;
            jd_to_gregorian(jd, out greg_y, out greg_m, out greg_d);
			bool leap = leap_gregorian(greg_y);
			year = greg_y - Saka;
			double greg = gregorian_to_jd(greg_y, 1, 1);
			double yday = (jd - greg);
			int Caitra = _if(leap, 31, 30);
			if (yday < start)
			{
				year--;
				yday = (yday + (double)Caitra + 155.0 + 90.0 + 10.0 + start);
			}
			yday = (yday - start);
			if (yday < (double)Caitra)
			{
				month = 1;
				day = iFloor((yday + 1.0));
			}
			else
			{
				int mday = iFloor((yday - (double)Caitra));
				if (mday < 155)
				{
					month = iFloor(((double)mday / 31.0)) + 2;
					day = mday % 31 + 1;
				}
				else
				{
					mday -= 155;
					month = iFloor(((double)mday / 30.0)) + 7;
					day = mday % 30 + 1;
				}
			}
		}

		public static double bahai_to_jd(int major, int cycle, int year, int month, int day)
		{
			int by = 0;
			int dummy = 0;
            jd_to_gregorian(2394646.5, out by, out dummy, out dummy);
			int gy = 361 * (major - 1) + 19 * (cycle - 1) + (year - 1) + by;
			return (gregorian_to_jd(gy, 3, 20) + (double)(19 * (month - 1)) + (double)_if(month != 20, 0, _if(leap_gregorian(gy + 1), -14, -15)) + (double)day);
		}

		public static void jd_to_bahai(double jd, ref int major, ref int cycle, ref int year, ref int month, ref int day)
		{
			jd = (Math.Floor(jd) + 0.5);
			int gy = 0;
			int dummy = 0;
            jd_to_gregorian(jd, out gy, out dummy, out dummy);
			int bstarty = 0;
            jd_to_gregorian(2394646.5, out bstarty, out dummy, out dummy);
			int bys = gy - (bstarty + _if(gregorian_to_jd(gy, 1, 1) <= jd && jd <= gregorian_to_jd(gy, 3, 20), 1, 0));
			major = iFloor(((double)bys / 361.0)) + 1;
			cycle = iFloor((_modf((double)bys, 361.0) / 19.0)) + 1;
			year = _modi((double)bys, 19.0) + 1;
			double days = (jd - bahai_to_jd(major, cycle, year, 1, 1));
			double bld = bahai_to_jd(major, cycle, year, 20, 1);
			month = _if(jd >= bld, 20, iFloor((days / 19.0)) + 1);
			day = iFloor((jd + 1.0 - bahai_to_jd(major, cycle, year, month, 1)));
		}

		public static string date_to_str(int aYear, int aMonth, int aDay, CalendarConverter.TDateEra aEra)
		{
			DateTimeFormatInfo DateTimeInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;
			string Result = string.Concat(new string[]
			{
				aDay.ToString(), 
				" ", 
				DateTimeInfo.AbbreviatedMonthNames[aMonth - 1], 
				" ", 
				aYear.ToString()
			});
			if (aEra != CalendarConverter.TDateEra.AD)
			{
				Result += " до н.э.";
			}
			return Result;
		}

		static CalendarConverter()
		{
			CalendarConverter.BahaiWeekdays = new string[]
			{
				"Джамаль", 
				"Камаль", 
				"Фидаль", 
				"Идаль", 
				"Истиджлаль", 
				"Истиклаль", 
				"Джалаль"
			};

			CalendarConverter.BahaiMonths = new string[]
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

			CalendarConverter.IndianCivilWeekdays = new string[]
			{
				"равивар", 
				"сомвар", 
				"мангалвар", 
				"будхвар", 
				"брихаспативар", 
				"шукрвар", 
				"шанивар"
			};

			CalendarConverter.IndianCivilMonths = new string[]
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

			CalendarConverter.PersianWeekdays = new string[]
			{
				"йекшанбе", 
				"душанбе", 
				"сешанбе", 
				"чахаршанбе", 
				"панджшанбе", 
				"джоме", 
				"шанбе"
			};

			CalendarConverter.PersianMonths = new string[]
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

			CalendarConverter.IslamicWeekdays = new string[]
			{
				"аль-ахад", 
				"аль-иснайн", 
				"ас-саласа'", 
				"аль-арба'а", 
				"аль-хамис", 
				"аль-джум'а", 
				"ас-сабт"
			};

			CalendarConverter.IslamicMonths = new string[]
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

			CalendarConverter.HebrewWeekdays = new string[]
			{
				"алеф", 
				"бейт", 
				"гимел", 
				"далет", 
				"хей", 
				"вав", 
				"зайин"
			};

			CalendarConverter.HebrewMonths = new string[]
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
