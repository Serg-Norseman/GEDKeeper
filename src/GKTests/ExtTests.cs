using System;
using GKCommon;
using GKCommon.GEDCOM.Enums;
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
		public void TSet_Tests()
		{
			EnumSet<GEDCOMRestriction> es = EnumSet<GEDCOMRestriction>.Create();
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
			Assert.AreEqual("00000101", test);
			
			EnumSet<GEDCOMRestriction> es2 = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnNone, GEDCOMRestriction.rnLocked);

			Assert.IsTrue(es.Equals(es2));
			Assert.IsFalse(es.Equals(null));
			
			Assert.IsTrue(es.Contains(GEDCOMRestriction.rnLocked));
			Assert.IsFalse(es.Contains(GEDCOMRestriction.rnPrivacy));

			EnumSet<GEDCOMRestriction> es3 = EnumSet<GEDCOMRestriction>.Create(GEDCOMRestriction.rnLocked);
			EnumSet<GEDCOMRestriction> es4 = es * es3;
			Assert.IsTrue(es.Contains(GEDCOMRestriction.rnLocked));
			
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
			long val = SysUtils.Trunc(495.575);
			Assert.AreEqual(val, 495);

			int ival = SysUtils.ParseInt("495", 0);
			Assert.AreEqual(ival, 495);

			double fval = SysUtils.ParseFloat("495.575", 0);
			Assert.AreEqual(fval, 495.575);

			string str;
			str = SysUtils.TrimLeft("	test1");
			Assert.AreEqual(str, "test1");

			str = SysUtils.TrimLeft(null);
			Assert.AreEqual(str, "");

			str = SysUtils.TrimRight("test2		");
			Assert.AreEqual(str, "test2");

			str = SysUtils.TrimChars("xyxyx test3", new char[] {'x', 'y'});
			Assert.AreEqual(str, " test3");

			string st = SysUtils.NumUpdate(9, 3);
			Assert.AreEqual(st, "009");
		}

		[Test]
		public void Calculator_Tests()
		{
			ExpCalculator calc = new ExpCalculator();
			Assert.IsNotNull(calc);
			
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