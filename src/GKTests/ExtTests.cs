using System;
using Ext.Utils;
using NUnit.Framework;

namespace GKTests
{
	[TestFixture]
	public class ExtTests
	{
		[Test]
		public void RomeTest()
		{
			Assert.AreEqual("VIII", RomeNumbers.GetRome(8), "RomeTest1");
			Assert.AreEqual("IX", RomeNumbers.GetRome(9), "RomeTest2");
			Assert.AreEqual("XXXI", RomeNumbers.GetRome(31), "RomeTest3");
			Assert.AreEqual("XLVI", RomeNumbers.GetRome(46), "RomeTest4");
			Assert.AreEqual("XCIX", RomeNumbers.GetRome(99), "RomeTest5");
			Assert.AreEqual("DLXXXIII", RomeNumbers.GetRome(583), "RomeTest6");
			Assert.AreEqual("DCCCLXXXVIII", RomeNumbers.GetRome(888), "RomeTest7");
			Assert.AreEqual("MDCLXVIII", RomeNumbers.GetRome(1668), "RomeTest8");
			Assert.AreEqual("MCMLXXXIX", RomeNumbers.GetRome(1989), "RomeTest9");
			Assert.AreEqual("MMMCMXCIX", RomeNumbers.GetRome(3999), "RomeTest10");
		}

		[Test]
		public void SCCryptTest()
		{
			string pw = "test password";
			string crypt = SCCrypt.scEncrypt(pw, unchecked((ushort)CRC32.CrcStr("test")));
			string pw1 = SCCrypt.scDecrypt(crypt, unchecked((ushort)CRC32.CrcStr("test")));
			
			Assert.AreEqual(pw, pw1, "pw == pw1");
		}
	}
}