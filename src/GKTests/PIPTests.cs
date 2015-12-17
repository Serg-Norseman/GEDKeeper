using System;
using NUnit.Framework;
using GKPedigreeImporterPlugin;

namespace GKTests
{
	[TestFixture]
	public class PIPTests
	{
		public PIPTests()
		{
		}

		[Test]
		public void ImpUtils_Valid_Tests()
		{
			bool res;
			string pid = "";

			res = ImpUtils.IsPersonLine_Konovalov("1. Ivan", ref pid);
			Assert.AreEqual("1.", pid, "[v1]");
			Assert.AreEqual(true, res, "[v1]");

			res = ImpUtils.IsPersonLine_Konovalov("2-1. Ivan", ref pid);
			Assert.AreEqual("2-1.", pid);
			Assert.AreEqual(true, res);

			res = ImpUtils.IsPersonLine_Konovalov("11-21. Ivan", ref pid);
			Assert.AreEqual("11-21.", pid);
			Assert.AreEqual(true, res);

			res = ImpUtils.IsPersonLine_Konovalov("11-21/1. Ivan", ref pid);
			Assert.AreEqual("11-21/1.", pid);
			Assert.AreEqual(true, res);

			res = ImpUtils.IsPersonLine_Konovalov("11-21/?. Ivan", ref pid);
			Assert.AreEqual("11-21/?.", pid, "[v2]");
			Assert.AreEqual(true, res, "[v2]");

			res = ImpUtils.IsPersonLine_Konovalov("11-21/1 (test+2, test). Ivan", ref pid);
			Assert.AreEqual("11-21/1 (test+2, test).", pid);
			Assert.AreEqual(true, res);



			string persId, parentId, marNum, extData;
			int pos;
			res = ImpUtils.ParsePersonLine_Konovalov("11-21/1 (test+2, test). Ivan", out persId, out parentId, out marNum, out extData, out pos);
			Assert.AreEqual("11", persId);
			Assert.AreEqual("21", parentId);
			Assert.AreEqual("1", marNum);
			Assert.AreEqual("(test+2, test)", extData);
			Assert.AreEqual(true, res);

			res = ImpUtils.ParsePersonLine_Konovalov("11-21/?. Ivan", out persId, out parentId, out marNum, out extData, out pos);
			Assert.AreEqual("11", persId);
			Assert.AreEqual("21", parentId);
			Assert.AreEqual("?", marNum);
			Assert.AreEqual("", extData);
			Assert.AreEqual(true, res);



			res = ImpUtils.IsPersonLine_DAboville("1. Ivan", ref pid);
			Assert.AreEqual("1.", pid, "[v2-1]");
			Assert.AreEqual(true, res, "[v2-1]");

			res = ImpUtils.IsPersonLine_DAboville("1.1. Ivan", ref pid);
			Assert.AreEqual("1.1.", pid, "[v2-2]");
			Assert.AreEqual(true, res, "[v2-2]");

			res = ImpUtils.IsPersonLine_DAboville("11.21.31.11. Ivan", ref pid);
			Assert.AreEqual("11.21.31.11.", pid, "[v2-3]");
			Assert.AreEqual(true, res, "[v2-3]");



			res = ImpUtils.ParsePersonLine_DAboville("1. Ivan", out persId, out parentId, out marNum, out extData, out pos);
			Assert.AreEqual("1.", persId);
			Assert.AreEqual("", parentId);
			Assert.AreEqual("", marNum);
			Assert.AreEqual("", extData);
			Assert.AreEqual(true, res);

			res = ImpUtils.ParsePersonLine_DAboville("11.21.31.11. Ivan", out persId, out parentId, out marNum, out extData, out pos);
			Assert.AreEqual("11.21.31.11.", persId);
			Assert.AreEqual("11.21.31.", parentId);
			Assert.AreEqual("", marNum);
			Assert.AreEqual("", extData);
			Assert.AreEqual(true, res);



			string spouse;
			res = ImpUtils.ParseSpouseLine("Ж: Ivanova", out spouse, out marNum, out extData, out pos);
			Assert.AreEqual("Ж", spouse, "[v3-1]");
			Assert.AreEqual("1", marNum, "[v3-1]");
			Assert.AreEqual("", extData);
			Assert.AreEqual(true, res, "[v3-1]");

			res = ImpUtils.ParseSpouseLine("Ж2 (test): Ivanova", out spouse, out marNum, out extData, out pos);
			Assert.AreEqual("Ж", spouse, "[v3-2]");
			Assert.AreEqual("2", marNum, "[v3-2]");
			Assert.AreEqual("(test)", extData, "[v3-2]");
			Assert.AreEqual(true, res, "[v3-2]");

			res = ImpUtils.ParseSpouseLine("Ж - Ivanova", out spouse, out marNum, out extData, out pos);
			Assert.AreEqual("Ж", spouse, "[v3-3]");
			Assert.AreEqual("1", marNum, "[v3-3]");
			Assert.AreEqual("", extData);
			Assert.AreEqual(true, res, "[v3-3]");

			res = ImpUtils.ParseSpouseLine("Ж3 (test2) - Ivanova", out spouse, out marNum, out extData, out pos);
			Assert.AreEqual("Ж", spouse, "[v3-4]");
			Assert.AreEqual("3", marNum, "[v3-4]");
			Assert.AreEqual("(test2)", extData, "[v3-4]");
			Assert.AreEqual(true, res, "[v3-4]");
		}

		[Test]
		public void ImpUtils_Invalid_Tests()
		{
			bool res;
			string pid = "";

			res = ImpUtils.IsPersonLine_Konovalov("-1. Ivan", ref pid);
			Assert.AreEqual(false, res);

			res = ImpUtils.IsPersonLine_Konovalov("1 Ivan", ref pid);
			Assert.AreEqual(false, res);

			res = ImpUtils.IsPersonLine_Konovalov("1-. Ivan", ref pid);
			Assert.AreEqual(false, res);

			res = ImpUtils.IsPersonLine_Konovalov("11-11 Ivan", ref pid);
			Assert.AreEqual(false, res);

			res = ImpUtils.IsPersonLine_Konovalov("1.2. Ivan", ref pid);
			Assert.AreEqual(false, res, "[i1]");

			res = ImpUtils.IsPersonLine_Konovalov("1.1.1. Ivan", ref pid);
			Assert.AreEqual(false, res, "[i2]");

			res = ImpUtils.IsPersonLine_Konovalov("11-21/. Ivan", ref pid);
			Assert.AreEqual(false, res);

			res = ImpUtils.IsPersonLine_Konovalov("11-21-31. Ivan", ref pid);
			Assert.AreEqual(false, res);

			res = ImpUtils.IsPersonLine_Konovalov("11-21-31 (. Ivan", ref pid);
			Assert.AreEqual(false, res);

			res = ImpUtils.IsPersonLine_Konovalov("11-21-31 (test) Ivan", ref pid);
			Assert.AreEqual(false, res);



			res = ImpUtils.IsPersonLine_DAboville("-1. Ivan", ref pid);
			Assert.AreEqual(false, res, "[i2-1]");

			res = ImpUtils.IsPersonLine_DAboville("1-1. Ivan", ref pid);
			Assert.AreEqual(false, res, "[i2-2]");

			res = ImpUtils.IsPersonLine_DAboville(".1. Ivan", ref pid);
			Assert.AreEqual(false, res, "[i2-3]");



			string spouse, marNum, extData;
			int pos;
			res = ImpUtils.ParseSpouseLine("Жена Ivanova", out spouse, out marNum, out extData, out pos);
			Assert.AreEqual(false, res, "[i3-1]");

			res = ImpUtils.ParseSpouseLine("Ж2 Ivanova", out spouse, out marNum, out extData, out pos);
			Assert.AreEqual(false, res, "[i3-2]");

			res = ImpUtils.ParseSpouseLine("Ж Ivanova", out spouse, out marNum, out extData, out pos);
			Assert.AreEqual(false, res, "[i3-3]");

			res = ImpUtils.ParseSpouseLine("Ж3 (test2 - Ivanova", out spouse, out marNum, out extData, out pos);
			Assert.AreEqual(false, res, "[i3-4]");
		}
	}
}
