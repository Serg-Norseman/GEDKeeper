using System;
using GKCommon;
using NUnit.Framework;

namespace CalendarConverterTests
{
	[TestFixture]
	public class ExtTests
	{
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
			Assert.AreEqual("27 сен 1990", s); // +

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