using System;
using Ext.Utils;
using GedCom551;
using NUnit.Framework;

namespace GKTests
{
	[TestFixture]
	public class ExtTests
	{
		[Test]
		public void RomeNumbers_Test()
		{
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
		public void SCCrypt_Test()
		{
			string pw = "test password";
			string crypt = SCCrypt.scEncrypt(pw, unchecked((ushort)CRC32.CrcStr("test")));
			string pw1 = SCCrypt.scDecrypt(crypt, unchecked((ushort)CRC32.CrcStr("test")));
			
			Assert.AreEqual(pw, pw1, "SCCrypt_Test");
		}

		[Test]
		public void EnumSet_Test()
		{
			//TGEDCOMRestriction
			EnumSet es = EnumSet.Create();
			Assert.IsTrue(es.IsEmpty());

			es.Include(new Enum[] {TGEDCOMRestriction.rnPrivacy, TGEDCOMRestriction.rnLocked});
			Assert.IsTrue(es.InSet(TGEDCOMRestriction.rnPrivacy));
			Assert.IsFalse(es.InSet(TGEDCOMRestriction.rnNone));
			Assert.IsFalse(es.IsEmpty());

			es.Exclude(TGEDCOMRestriction.rnPrivacy);
			Assert.IsFalse(es.InSet(TGEDCOMRestriction.rnPrivacy));
			Assert.IsTrue(es.InSet(TGEDCOMRestriction.rnLocked));
			
			es = EnumSet.Create(new Enum[] {TGEDCOMRestriction.rnNone, TGEDCOMRestriction.rnLocked});
			Assert.IsTrue(es.InSet(TGEDCOMRestriction.rnNone));
			Assert.IsTrue(es.InSet(TGEDCOMRestriction.rnLocked));
			
			string test = es.ToString();
			Assert.AreEqual(test, "00000101");
		}

		[Test]
		public void IndistinctMatching_Test()
		{
			int res1, res2;

			res1 = IndistinctMatching.LevenshteinDistance("Иванов", "Иванов");
			Assert.AreEqual(res1, 0);
			res1 = IndistinctMatching.LevenshteinDistance("Иванво", "Иванов");
			Assert.AreEqual(res1, 2);

			res2 = IndistinctMatching.DamerauLevenshteinDistance("Иванов", "Иванов");
			Assert.AreEqual(res2, 0);
			res2 = IndistinctMatching.DamerauLevenshteinDistance("Иванво", "Иванов");
			Assert.AreEqual(res2, 1);
			
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
		public void StringList_Test()
		{
			
		}

		[Test]
		public void SysUtils_Test()
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
		public void Calculator_Test()
		{
			TCalculator calc = new TCalculator();
			Assert.IsNotNull(calc);

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
		}

		[Test]
		public void TList_Test()
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