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
using GKCommon;
using GKCore;
using NUnit.Framework;

namespace GKTests
{
    [TestFixture]
    public class ExtTests
    {
        [Test]
        public void SCCrypt_Tests()
        {
            const string pw = "test password";
            string crypt = SCCrypt.scEncrypt(pw, unchecked((ushort)CRC32.CrcStr("test")));
            string pw1 = SCCrypt.scDecrypt(crypt, unchecked((ushort)CRC32.CrcStr("test")));

            Assert.AreEqual(pw, pw1, "SCCrypt_Test");
        }

        
        /*[Test]
		public void Sort_PerfTest()
		{
			Random rnd = new Random();

			List<ValItem> listQS = new List<ValItem>();
			List<ValItem> listMS = new List<ValItem>();
			List<ValItem> listTS = new List<ValItem>();
			List<ValItem> listCS = new List<ValItem>();

			for (int i = 0; i < 1000000; i++)
			{
				double val = rnd.NextDouble();

				listTS.Add(new ValItem(val));
				listQS.Add(new ValItem(val));
				listMS.Add(new ValItem(val));
				listCS.Add(new ValItem(val));
			}

			listCS.Sort(CompareItems);

			SortHelper.QuickSort(listQS, CompareItems);

			SortHelper.MergeSort(listMS, CompareItems);

			ExtUtils.ListTimSort<ValItem>.Sort(listTS, CompareItems);
		}

		private class ValItem
		{
			public double Value;

			public ValItem(double value)
			{
				this.Value = value;
			}
		}

		private int CompareItems(ValItem item1, ValItem item2)
		{
			return item1.Value.CompareTo(item2.Value);
		}*/
        
        
        [Test]
        public void SysUtils_Tests()
        {
            Assert.AreEqual(true, GKUtils.IsSetBit(3, 0));
            Assert.AreEqual(true, GKUtils.IsSetBit(3, 1));
            Assert.AreEqual(false, GKUtils.IsSetBit(3, 4));
        }

        [Test]
        public void ExpCalculator_Tests()
        {
            ExpCalculator calc = new ExpCalculator();
            Assert.IsNotNull(calc);
            
            calc.CaseSensitive = false;
            Assert.AreEqual(false, calc.CaseSensitive);
            
            calc.OnGetVar += GetVarEventHandler;

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("12+"); }); // syntax error
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("(12+"); }); // syntax error
            //Assert.Throws(typeof(CalculateException), () => { calc.Calc("5 + 0x"); }); // syntax error
            Assert.Throws(typeof(CalculateException), () => { calc.Calc(")"); }); // syntax error
            
            double val = calc.Calc("2 + 7.703 - 3");
            Assert.AreEqual(Math.Round(val, 3), 6.703);
            
            val = calc.Calc("2**3");
            Assert.AreEqual(val, 8.0);
            
            val = calc.Calc("2 * 3");
            Assert.AreEqual(val, 6.0);
            
            val = calc.Calc("3 / 2");
            Assert.AreEqual(val, 1.5);
            
            val = calc.Calc("3 % 2");
            Assert.AreEqual(val, 1.0);
            
            val = calc.Calc("3 %% 2");
            Assert.AreEqual(val, 150.0);

            Assert.AreEqual(-2.0005, calc.Calc("-2.0005"));

            Assert.AreEqual(-2.0e+1, calc.Calc("-2.0e+1"));
            Assert.AreEqual(-2.0e-1, calc.Calc("-2.0e-1"));
            Assert.AreEqual(-2.0e0, calc.Calc("-2.0e0"));

            // variables
            calc.ClearVars();
            
            calc.SetVar("a", 10);
            Assert.AreEqual(10, calc.GetVar("a"));
            calc.SetVar("b", 2);
            Assert.AreEqual(2, calc.GetVar("b"));
            calc.SetVar("c", 0.75);
            Assert.AreEqual(0.75, calc.GetVar("c"));
            
            val = calc.Calc("a+b+c");
            Assert.AreEqual(12.75, val);
            
            val = calc.Calc("15 / ((a+b)-c)");
            Assert.AreEqual(1.333, Math.Round(val, 3));
            
            calc.SetVar("a", 20);
            Assert.AreEqual(20, calc.GetVar("a"));
            
            val = calc.Calc("a+b+c");
            Assert.AreEqual(22.75, val);
            
            val = calc.Calc("d=a+b+c");
            Assert.AreEqual(22.75, calc.GetVar("d"));
            
            val = calc.Calc("d = a + b + c; e = d * 2");
            Assert.AreEqual(22.75, calc.GetVar("d"));
            Assert.AreEqual(45.5, calc.GetVar("e"));
            
            // functions
            val = calc.Calc("round(12.378)");
            Assert.AreEqual(12.0, val);
            
            val = calc.Calc("round(12.578)");
            Assert.AreEqual(13.0, val);
            
            val = calc.Calc("trunc(12.578)");
            Assert.AreEqual(12.0, val);
            
            val = calc.Calc("int(12.578)");
            Assert.AreEqual(12.0, val);
            
            val = calc.Calc("frac(12.578)");
            Assert.AreEqual(0.578, Math.Round(val, 3));
            
            val = calc.Calc("sin(30`)");
            Assert.AreEqual(0.500, Math.Round(val, 3));
            
            val = calc.Calc("cos(30`)");
            Assert.AreEqual(0.866, Math.Round(val, 3));
            
            val = calc.Calc("tan(30`)");
            Assert.AreEqual(0.577, Math.Round(val, 3));
            
            val = calc.Calc("atan(30`)");
            Assert.AreEqual(0.482, Math.Round(val, 3));
            
            val = calc.Calc("exp(5)");
            Assert.AreEqual(148.413, Math.Round(val, 3));
            
            val = calc.Calc("ln(117)");
            Assert.AreEqual(4.762, Math.Round(val, 3));
            
            val = calc.Calc("sign(-15)");
            Assert.AreEqual(-1, Math.Round(val, 3));
            
            val = calc.Calc("sign(2)");
            Assert.AreEqual(+1, Math.Round(val, 3));

            val = calc.Calc("pi * 2");
            Assert.AreEqual(6.283, Math.Round(val, 3));

            val = calc.Calc("e");
            Assert.AreEqual(2.718, Math.Round(val, 3));
            
            // logic
            val = calc.Calc("2 < 3");
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("2 > 3");
            Assert.AreEqual(0, Math.Round(val, 0));
            val = calc.Calc("3 <= 3");
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("2 >= 3");
            Assert.AreEqual(0, Math.Round(val, 0));
            val = calc.Calc("3 == 3");
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("2 != 3");
            Assert.AreEqual(1, Math.Round(val, 0));
            
            // misc
            val = calc.Calc("2 ^ 3"); // xor
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("5 | 2"); // or
            Assert.AreEqual(7, Math.Round(val, 0));
            val = calc.Calc("9 & 5"); // and
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("~15"); // inv
            Assert.AreEqual(-16, Math.Round(val, 0));
            val = calc.Calc("!-15"); // not
            Assert.AreEqual(1, Math.Round(val, 0));

            // vars
            val = calc.Calc("15 - alpha");
            Assert.AreEqual(0, Math.Round(val, 0));

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("15 - beta"); });
            
            calc.OnGetVar -= GetVarEventHandler;
            
            // numbers
            val = calc.Calc("1537");
            Assert.AreEqual(1537, Math.Round(val, 0));
            val = calc.Calc("0b11000000001");
            Assert.AreEqual(1537, Math.Round(val, 0));
            val = calc.Calc("0x601");
            Assert.AreEqual(1537, Math.Round(val, 0));
            val = calc.Calc("$601");
            Assert.AreEqual(1537, Math.Round(val, 0));
            val = calc.Calc("601h");
            Assert.AreEqual(1537, Math.Round(val, 0));
            //val = calc.Calc("5.25e+2");
            //Assert.AreEqual(525, Math.Round(val, 0));

            val = calc.Calc("if(3 == 3; 2; 3)");
            Assert.AreEqual(2, Math.Round(val, 0));

            val = calc.Calc("if(2 == 3; 2; 3)");
            Assert.AreEqual(3, Math.Round(val, 0));
            
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("if(2 == 3)"); }); // syntax error
        }

        private static bool GetVarEventHandler(object sender, string varName, ref double varValue)
        {
            if (varName.Equals("alpha")) {
                varValue = 15.0;
                return true;
            }

            return false;
        }

        /*[Test]
		public void Calendar_PerfTest()
		{
			int y = 1990, m = 10, d = 10;
			double jd;

			for (int i = 0; i < 1000000; i++) {
				jd = CalendarConverter.gregorian_to_jd(y, m, d);
				jd = CalendarConverter.gregorian_to_jd2(y, m, d);
				jd = CalendarConverter.gregorian_to_jd3(y, m, d);
			}
		}*/
        
        [Test]
        public void Calendar_Tests()
        {
            DateTime gdt = new DateTime(1990, 10, 10);
            string s;

            double jd = CalendarConverter.gregorian_to_jd2(gdt.Year, gdt.Month, gdt.Day);
            Assert.AreEqual(2448175, jd); // 

            jd = CalendarConverter.gregorian_to_jd2(-4713, 11, 24);
            Assert.AreEqual(0.0, jd); // 

            
            jd = CalendarConverter.gregorian_to_jd(-4713, 11, 24);
            Assert.AreEqual(0.5, jd); // bad result! needs 0.0f!
            
            jd = CalendarConverter.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);
            Assert.AreEqual(2448174.5, jd); // not checked!
            
            
            int year, month, day;
            CalendarConverter.jd_to_julian(jd, out year, out month, out day);
            s = CalendarData.date_to_str(year, month, day, CalendarData.DateEra.AD);
            Assert.AreEqual("27 09 1990", s); // +

            CalendarConverter.jd_to_hebrew(jd, out year, out month, out day);
            s = day.ToString() + " ";
            s += CalendarData.HebrewMonths[month - 1];
            s = s + " " + year.ToString() + ", " + CalendarData.HebrewWeekdays[CalendarConverter.jwday(jd)];
            Assert.AreEqual("21 Тишрей 5751, далет", s); // +

            CalendarConverter.jd_to_islamic(jd, out year, out month, out day);
            s = day.ToString() + " ";
            s += CalendarData.IslamicMonths[month - 1];
            s = s + " " + year.ToString() + ", йаум " + CalendarData.IslamicWeekdays[CalendarConverter.jwday(jd)];
            Assert.AreEqual("20 рабии`у ль-авваль 1411, йаум аль-арба'а", s); // +

            CalendarConverter.jd_to_persian(jd, out year, out month, out day);
            s = day.ToString() + " ";
            s += CalendarData.PersianMonths[month - 1];
            s = s + " " + year.ToString() + ", " + CalendarData.PersianWeekdays[CalendarConverter.jwday(jd)];
            Assert.AreEqual("18 Мехр 1369, чахаршанбе", s); // +

            CalendarConverter.jd_to_indian_civil(jd, out year, out month, out day);
            s = day.ToString() + " ";
            s += CalendarData.IndianCivilMonths[month - 1];
            s = s + " " + year.ToString() + ", " + CalendarData.IndianCivilWeekdays[CalendarConverter.jwday(jd)];
            Assert.AreEqual("18 Азвина 1912, будхвар", s); // +

            int major, cycle;
            CalendarConverter.jd_to_bahai(jd, out major, out cycle, out year, out month, out day);
            s = "Кулл-и Шай' " + major.ToString() + ", Вахид " + cycle.ToString() + ", ";
            s = s + day.ToString() + " ";
            s += CalendarData.BahaiMonths[month - 1];
            s = s + " " + year.ToString() + ", " + CalendarData.BahaiWeekdays[CalendarConverter.jwday(jd)];
            Assert.AreEqual("Кулл-и Шай' 1, Вахид 8, 14 Машиййат 14, Идаль", s); // ???
        }
        
        [Test]
        public void CalendarGK_Tests()
        {
            double jd;
            int year, month, day;
            
            const double needJD = 2448174.5; // 1990-10-10 [g], 1990-09-27 [j], 5751-07-21 [h]
            
            //for (int i = 0; i < 1000000; i++) {
            jd = CalendarConverter.gregorian_to_jd(-4713, 11, 24);
            Assert.AreEqual(0.5, jd); // bad!
            
            jd = CalendarConverter.gregorian_to_jd(1990, 10, 10);
            Assert.AreEqual(needJD, jd);
            
            jd = CalendarConverter.julian_to_jd(1990, 09, 27);
            Assert.AreEqual(needJD, jd);

            jd = CalendarConverter.hebrew_to_jd(5751, 07, 21);
            Assert.AreEqual(needJD, jd);
            //}


            /*jd = CalendarConverter.julian_to_jd(1990, 09, 00);
			CalendarConverter.jd_to_julian(jd, out year, out month, out day);
			Assert.AreEqual(1990, year, "j2jd 1");
			Assert.AreEqual(09, month, "j2jd 2");
			Assert.AreEqual(00, day, "j2jd 3");*/


            jd = CalendarConverter.gregorian_to_jd2(1990, 10, 10);
            CalendarConverter.jd_to_gregorian2(jd, out year, out month, out day);
            Assert.AreEqual(1990, year, "g2jd 1");
            Assert.AreEqual(10, month, "g2jd 2");
            Assert.AreEqual(10, day, "g2jd 3");
        }

//		public void MyTestFunc1(
        //            [Values(1, 2, 5)]int x,
        //            [Values("hello", "buy")]string s)
//		{
//			Assert.IsTrue(x < 10);
//		}
//
//		[Test]
        //        public void MyTestFunc2(
        //            [Range(1, 100, 2)]int x,
        //            [Values("hello", "buy")]string s)
        //        {
        //            Assert.IsTrue(x < 50);
        //        }
//
//		public void MyTestFunc3(
        //            [Random(100)]int x,
        //            [Values("hello", "buy")]string s)
//		{
//
//		}

    }
}
