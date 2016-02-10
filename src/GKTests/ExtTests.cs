using System;
using System.Drawing;

using BSLib;
using BSLib.SmartGraph;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using NUnit.Framework;

namespace GKTests
{
	[TestFixture]
	public class ExtTests
	{
		[Test]
		public void RomeNumbers_Tests()
		{
			Assert.AreEqual("VI", RomeNumbers.GetRome(6), "RomeTest_00");
			Assert.AreEqual("VIII", RomeNumbers.GetRome(8), "RomeTest_01");
			Assert.AreEqual("IX", RomeNumbers.GetRome(9), "RomeTest_02");
			Assert.AreEqual("XXXI", RomeNumbers.GetRome(31), "RomeTest_03");
			Assert.AreEqual("XLVI", RomeNumbers.GetRome(46), "RomeTest_04");
			Assert.AreEqual("XCIX", RomeNumbers.GetRome(99), "RomeTest_05");
			Assert.AreEqual("DLXXXIII", RomeNumbers.GetRome(583), "RomeTest_06");
			Assert.AreEqual("DCCCLXXXVIII", RomeNumbers.GetRome(888), "RomeTest_07");
			Assert.AreEqual("MDCLXVIII", RomeNumbers.GetRome(1668), "RomeTest_08");
			Assert.AreEqual("MCMLXXXIX", RomeNumbers.GetRome(1989), "RomeTest_09");
			Assert.AreEqual("MMMCMXCIX", RomeNumbers.GetRome(3999), "RomeTest_10");
		}

		[Test]
		public void SCCrypt_Tests()
		{
			const string pw = "test password";
			string crypt = SCCrypt.scEncrypt(pw, unchecked((ushort)CRC32.CrcStr("test")));
			string pw1 = SCCrypt.scDecrypt(crypt, unchecked((ushort)CRC32.CrcStr("test")));
			
			Assert.AreEqual(pw, pw1, "SCCrypt_Test");
		}

		[Test]
		public void EnumSet_Tests()
		{
			EnumSet<GEDCOMRestriction> es = EnumSet<GEDCOMRestriction>.Create();
			Assert.IsTrue(es.IsEmpty());

			es.Include(null);
			Assert.IsTrue(es.IsEmpty());
			
			es.Include(GEDCOMRestriction.rnPrivacy, GEDCOMRestriction.rnLocked);
			Assert.IsTrue(es.Contains(GEDCOMRestriction.rnPrivacy));
			Assert.IsFalse(es.Contains(GEDCOMRestriction.rnNone));
			Assert.IsFalse(es.IsEmpty());

			es.Exclude(GEDCOMRestriction.rnPrivacy);
			Assert.IsFalse(es.Contains(GEDCOMRestriction.rnPrivacy));
			Assert.IsTrue(es.Contains(GEDCOMRestriction.rnLocked));
			
			es = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnNone, GEDCOMRestriction.rnLocked);
			Assert.IsTrue(es.Contains(GEDCOMRestriction.rnNone));
			Assert.IsTrue(es.Contains(GEDCOMRestriction.rnLocked));
			
			string test = es.ByteToStr((int)0);
			Assert.AreEqual("00000011", test);
			
			// clone test
			EnumSet<GEDCOMRestriction> copy = (EnumSet<GEDCOMRestriction>)es.Clone();
			test = copy.ByteToStr((int)0);
			Assert.AreEqual("00000011", test);
			
			// clear test
			copy.Clear();
			Assert.IsTrue(copy.IsEmpty());
			
			//
			EnumSet<GEDCOMRestriction> es2 = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnNone, GEDCOMRestriction.rnLocked);

			Assert.IsTrue(es.Equals(es2));
			Assert.IsFalse(es.Equals(null));
			
			Assert.IsTrue(es.Contains(GEDCOMRestriction.rnLocked));
			Assert.IsFalse(es.Contains(GEDCOMRestriction.rnPrivacy));

			EnumSet<GEDCOMRestriction> es3 = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnLocked);
			EnumSet<GEDCOMRestriction> es4 = es * es3;
            Assert.IsTrue(es4.Contains(GEDCOMRestriction.rnLocked));
			
			es = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnNone);
			es2 = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnLocked);
			Assert.IsTrue(es != es2);
			
			es = es + es2;
			es3 = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnNone, GEDCOMRestriction.rnLocked);
			Assert.IsTrue(es.Equals(es3));
			
			Assert.IsTrue(es3.ContainsAll(GEDCOMRestriction.rnNone, GEDCOMRestriction.rnLocked));
			Assert.IsFalse(es3.ContainsAll(GEDCOMRestriction.rnNone, GEDCOMRestriction.rnPrivacy));
			Assert.IsTrue(es3.HasIntersect(GEDCOMRestriction.rnNone, GEDCOMRestriction.rnPrivacy));
			Assert.IsFalse(es3.HasIntersect(GEDCOMRestriction.rnPrivacy));
			
			es = es - es2;
			es3 = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnNone);
			Assert.IsTrue(es == es3);
		}

		[Test]
		public void IndistinctMatching_Tests()
		{
			int res1, res2;

			res1 = IndistinctMatching.LevenshteinDistance("Иванов", "Иванов");
			Assert.AreEqual(0, res1);
			res1 = IndistinctMatching.LevenshteinDistance("Иванво", "Иванов");
			Assert.AreEqual(2, res1);

			res2 = IndistinctMatching.DamerauLevenshteinDistance("Иванов", "Иванов");
			Assert.AreEqual(0, res2);
			res2 = IndistinctMatching.DamerauLevenshteinDistance("Иванво", "Иванов");
			Assert.AreEqual(1, res2);
			
			double sim = IndistinctMatching.GetSimilarity("Иванво", "Иванов");
			Assert.GreaterOrEqual(sim, 0.8333);
		}

		[Test]
		public void IndistinctMatching_PerfTest1()
		{
			int res1, res2;

			for (int i = 1; i < 10000; i++) {
				res1 = IndistinctMatching.LevenshteinDistance("Иван", "Иванов");
				res2 = IndistinctMatching.DamerauLevenshteinDistance("Иван", "Иванов");
			}
		}

		[Test]
		public void IndistinctMatching_PerfTest2()
		{
			int res1, res2;

			for (int i = 1; i < 10000; i++) {
				res1 = IndistinctMatching.LevenshteinDistance("Иванво", "Иванов");
				res2 = IndistinctMatching.DamerauLevenshteinDistance("Иванво", "Иванов");
			}
		}

		
		/*[Test]
		public void Sort_PerfTest()
		{
			Random rnd = new Random();
			
			ExtList<ValItem> listQS = new ExtList<ExtTests.ValItem>();
			ExtList<ValItem> listMS = new ExtList<ExtTests.ValItem>();
			System.Collections.Generic.List<ValItem> listTS = new System.Collections.Generic.List<ValItem>();
			System.Collections.Generic.List<ValItem> listCS = new System.Collections.Generic.List<ValItem>();
			
			for (int i = 0; i < 100000; i++) {
				double val = rnd.NextDouble();
				
				listTS.Add(new ValItem(val));
				listQS.Add(new ValItem(val));
				listMS.Add(new ValItem(val));
				listCS.Add(new ValItem(val));
			}
			
			listQS.QuickSort(CompareItems);
			
			listMS.MergeSort(CompareItems);
			
			ExtUtils.ListTimSort<ValItem>.Sort(listTS, CompareItems);
			
			listCS.Sort(CompareItems);
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
		public void StringList_Tests()
		{
			string[] list = new string[4] { "The", "string", "list", "test" };
			
			StringList strList = new StringList(list);
			Assert.AreEqual("The", strList[0]);
			Assert.AreEqual("string", strList[1]);
			Assert.AreEqual("list", strList[2]);
			Assert.AreEqual("test", strList[3]);
			
			strList.Exchange(1, 2);
			Assert.AreEqual("string", strList[2]);
			Assert.AreEqual("list", strList[1]);
			
			strList.Clear();
		}

		[Test]
		public void SysUtils_Tests()
		{
			Assert.AreEqual(true, SysUtils.IsSetBit(3, 0));
			Assert.AreEqual(true, SysUtils.IsSetBit(3, 1));
			Assert.AreEqual(false, SysUtils.IsSetBit(3, 4));

			int ival = ConvHelper.ParseInt("495", 0);
			Assert.AreEqual(ival, 495);

			double fval = ConvHelper.ParseFloat("495.575", 0);
			Assert.AreEqual(fval, 495.575);

			string st = ConvHelper.AdjustNum(9, 3);
			Assert.AreEqual(st, "009");
		}

		[Test]
		public void ExpCalculator_Tests()
		{
			ExpCalculator calc = new ExpCalculator();
			Assert.IsNotNull(calc);
			
			calc.CaseSensitive = false;
			Assert.AreEqual(false, calc.CaseSensitive);
			
			calc.OnGetVar += this.GetVarEventHandler;

			Assert.Throws(typeof(CalculateException), () => { calc.Calc("12+"); }); // syntax error
			Assert.Throws(typeof(CalculateException), () => { calc.Calc("(12+"); }); // syntax error
			//Assert.Throws(typeof(CalculateException), () => { calc.Calc("5 + 0x"); }); // syntax error
			
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
			
			val = calc.Calc("-2");
			Assert.AreEqual(val, -2.0);
			
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
			
			calc.OnGetVar -= this.GetVarEventHandler;
			
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

		private bool GetVarEventHandler(object sender, string varName, ref double varValue)
		{
			if (varName.Equals("alpha")) {
				varValue = 15.0;
				return true;
			} else return false;
		}

		[Test]
		public void TList_Tests()
		{
			
		}

		[Test]
		public void Calendar_PerfTest()
		{
			int y = 1990, m = 10, d = 10;
			double jd;

			for (int i = 0; i < 10000000; i++) {
				jd = CalendarConverter.gregorian_to_jd(y, m, d);
				jd = CalendarConverter.gregorian_to_jd2(y, m, d);
			}
		}
		
		[Test]
		public void Calendar_Tests()
		{
			DateTime gdt = new DateTime(1990, 10, 10);
			string s;

			double jd = CalendarConverter.gregorian_to_jd2(gdt.Year, gdt.Month, gdt.Day);
			Assert.AreEqual(2448175, jd); // 

			jd = CalendarConverter.gregorian_to_jd(-4713, 11, 24);
			Assert.AreEqual(0.5, jd); // bad result! needs 0.0f!
			
			jd = CalendarConverter.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);
			Assert.AreEqual(2448174.5, jd); // not checked!
			
			int year = 0;
			int month = 0;
			int day = 0;
            CalendarConverter.jd_to_julian(jd, out year, out month, out day);
			s = CalendarData.date_to_str(year, month, day, CalendarConverter.DateEra.AD);
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

			int major = 0;
			int cycle = 0;
            CalendarConverter.jd_to_bahai(jd, out major, out cycle, out year, out month, out day);
			s = "Кулл-и Шай' " + major.ToString() + ", Вахид " + cycle.ToString() + ", ";
			s = s + day.ToString() + " ";
			s += CalendarData.BahaiMonths[month - 1];
			s = s + " " + year.ToString() + ", " + CalendarData.BahaiWeekdays[CalendarConverter.jwday(jd)];
			Assert.AreEqual("Кулл-и Шай' 1, Вахид 8, 14 Машиййат 14, Идаль", s); // ???
		}

		[Test]
		public void Graph_Tests()
		{
			Vertex vertex = new Vertex();
			Assert.IsNotNull(vertex);
			
			Vertex vertex2 = new Vertex();
			Assert.AreNotEqual(0, vertex.CompareTo(vertex2));
			Assert.Throws(typeof(ArgumentException), () => { vertex.CompareTo(null); });
			
			Assert.Throws(typeof(ArgumentNullException), () => { new Edge(null, vertex2, 1, null); });
			Assert.Throws(typeof(ArgumentNullException), () => { new Edge(vertex, null, 1, null); });
			
			Edge edge = new Edge(vertex, vertex2, 1, null);
			Assert.IsNotNull(edge);
			Assert.AreEqual(1, edge.Cost);
			Assert.AreEqual(vertex, edge.Source);
			Assert.AreEqual(vertex2, edge.Target);

			Assert.AreNotEqual(0, edge.CompareTo(new Edge(vertex, vertex2, 1, null)));
			Assert.Throws(typeof(ArgumentException), () => { edge.CompareTo(null); });
			
			IVertex vert1 = ((IEdge)edge).Source;
			Assert.AreEqual(vertex, vert1);
			IVertex vert2 = ((IEdge)edge).Target;
			Assert.AreEqual(vertex2, vert2);
			
			using (Graph graph = new Graph())
			{
				Assert.IsNotNull(graph);
				
				vert1 = graph.AddVertex(null);
				Assert.IsNotNull(vert1);
				graph.DeleteVertex(vert1);
				
				vert1 = graph.AddVertex("test", null);
				Assert.IsNotNull(vert1);
				
				vert2 = graph.FindVertex("test");
				Assert.AreEqual(vert1, vert2);
				
				graph.DeleteVertex(vert1);
				
				vert1 = graph.AddVertex("src", null);
				vert2 = graph.AddVertex("tgt", null);
				IEdge edge3 = graph.AddDirectedEdge("src", "tgt", 1, null);
				Assert.IsNotNull(edge3);
				graph.DeleteEdge(edge3);
				
				edge3 = graph.AddDirectedEdge("1", "2", 1, null);
				Assert.IsNull(edge3);
				
				bool res = graph.AddUndirectedEdge(vert1, vert2, 1, null, null);
				Assert.AreEqual(true, res);
				
				graph.Clear();
			}
		}

		[Test]
		public void ExtRect_Tests()
		{
			ExtRect rt = ExtRect.Create(0, 0, 9, 9);

			Assert.AreEqual(0, rt.Left);
			Assert.AreEqual(0, rt.Top);
			Assert.AreEqual(9, rt.Right);
			Assert.AreEqual(9, rt.Bottom);
			Assert.AreEqual(10, rt.GetHeight());
			Assert.AreEqual(10, rt.GetWidth());

			rt = ExtRect.CreateBounds(0, 0, 10, 10);

			Assert.AreEqual(0, rt.Left);
			Assert.AreEqual(0, rt.Top);
			Assert.AreEqual(9, rt.Right);
			Assert.AreEqual(9, rt.Bottom);
			Assert.AreEqual(10, rt.GetHeight());
			Assert.AreEqual(10, rt.GetWidth());

			Assert.AreEqual("{X=0,Y=0,Width=10,Height=10}", rt.ToString());

			Assert.IsTrue(rt.Contains(5, 5));
			
			rt.Inflate(3, -2);
			Assert.AreEqual("{X=3,Y=-2,Width=4,Height=14}", rt.ToString());
			
			rt.Offset(2, 5);
			Assert.AreEqual("{X=5,Y=3,Width=4,Height=14}", rt.ToString());
			
			rt = ExtRect.CreateEmpty();
			Assert.IsTrue(rt.IsEmpty());

			Assert.IsFalse(rt.Contains(5, 5));
			
			Rectangle rect = rt.ToRectangle();
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