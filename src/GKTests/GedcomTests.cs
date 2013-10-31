using System;
using GedCom551;
using NUnit.Framework;

namespace GKTests
{
	[TestFixture]
	public class GedcomTests
	{
		[Test]
		public void GEDCOMUtilsTests()
		{
			Assert.AreEqual("I12", GEDCOMUtils.CleanXRef("@I12@"), "CleanXRef(@I12@)");
			Assert.AreEqual("@I12@", GEDCOMUtils.EncloseXRef("I12"), "EncloseXRef(I12)");

			//
			string s1 = "abcd 12345 efgh";
			string s2, st;
			s2 = GEDCOMUtils.ExtractString(s1, out st, "");
			Assert.AreEqual(st, "abcd");
			Assert.AreEqual(s2, " 12345 efgh");
			
			s2 = GEDCOMUtils.ExtractDelimiter(s2, 0);
			Assert.AreEqual(s2, "12345 efgh");

			// проверить, что нормально отработает, если задан максимум
			s1 = GEDCOMUtils.ExtractDelimiter("    abrvalg", 2);
			Assert.AreEqual(s1, "  abrvalg");
			
			int N;
			s2 = GEDCOMUtils.ExtractNumber(s2, out N, true, 0);
			Assert.AreEqual(s2, " efgh");
			Assert.AreEqual(N, 12345);

			string xref;
			s2 = GEDCOMUtils.ExtractXRef("@I101@ sample", out xref, true, "");
			Assert.AreEqual(s2, " sample");
			Assert.AreEqual(xref, "I101");

			//
			Assert.IsFalse(GEDCOMUtils.IsDigit('F'), "IsDigit(F)");
			Assert.IsTrue(GEDCOMUtils.IsDigit('9'), "IsDigit(9)");

			Assert.IsFalse(GEDCOMUtils.IsDigits("f09"), "IsDigits(f09)");
			Assert.IsTrue(GEDCOMUtils.IsDigits("99"), "IsDigits(99)");

			Assert.AreEqual("M", GEDCOMUtils.GetSexStr(TGEDCOMSex.svMale), "GetSexStr(svMale)");
			Assert.AreEqual("F", GEDCOMUtils.GetSexStr(TGEDCOMSex.svFemale), "GetSexStr(svFemale)");
			Assert.AreEqual("U", GEDCOMUtils.GetSexStr(TGEDCOMSex.svUndetermined), "GetSexStr(svUndetermined)");
			Assert.AreEqual("", GEDCOMUtils.GetSexStr(TGEDCOMSex.svNone), "GetSexStr(svNone)");
		}

		[Test]
		public void GEDCOMFactoryTests()
		{
			GEDCOMFactory f = GEDCOMFactory.GetInstance();
			Assert.IsNotNull(f, "f != null");
			
			f.RegisterTag("DATE", TGEDCOMDateValue.Create);

			TGEDCOMTag tag = f.CreateTag(null, null, "DATE", "");
			Assert.IsNotNull(tag, "tag != null");

			tag = f.CreateTag(null, null, "TEST", "");
			Assert.IsNull(tag, "tag == null");
		}

		[Test]
		public void GEDCOMDateTests1()
		{
			TGEDCOMChangeDate cd = new TGEDCOMChangeDate(null, null, "CHAN", "");
			Assert.IsNotNull(cd, "cd != null");

			DateTime dtNow = DateTime.Now;
			dtNow = dtNow.AddTicks(-dtNow.Ticks % 10000000);
			cd.ChangeDateTime = dtNow;
			
			DateTime dtx = cd.ChangeDateTime;
			Assert.IsTrue(dtx.Equals(dtNow), "cd.ChangeDateTime == dtNow");
		}

		[Test]
		public void GEDCOMDateTests2()
		{
			TGEDCOMDateExact dtx1 = new TGEDCOMDateExact(null, null, "DATE", "20 JAN 2013");
			Assert.IsNotNull(dtx1, "dtx1 != null");

			DateTime dt = DateTime.Parse("20.01.2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
		}

		[Test]
		public void GEDCOMDateTests3()
		{
			TGEDCOMDateValue dtx1 = new TGEDCOMDateValue(null, null, "DATE", "20 JAN 2013");
			Assert.IsNotNull(dtx1, "dtx1 != null");

			DateTime dt = DateTime.Parse("20.01.2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
			
			dtx1.ParseString("INT 20 JAN 2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
			
			string st;
			
			st = "ABT 20 JAN 2013";
			dtx1.ParseString(st);
			Assert.IsTrue(dtx1.Date.Equals(dt));
			Assert.AreEqual(st, dtx1.StringValue);
			Assert.AreEqual(((TGEDCOMDateApproximated)dtx1.Value).Approximated, TGEDCOMApproximated.daAbout);
			
			st = "CAL 20 JAN 2013";
			dtx1.ParseString(st);
			Assert.AreEqual(dtx1.Date, dt);
			Assert.AreEqual(st, dtx1.StringValue);
			Assert.AreEqual(((TGEDCOMDateApproximated)dtx1.Value).Approximated, TGEDCOMApproximated.daCalculated);
			
			st = "EST 20 JAN 2013";
			dtx1.ParseString(st);
			Assert.AreEqual(dtx1.Date, dt);
			Assert.AreEqual(st, dtx1.StringValue);
			Assert.AreEqual(((TGEDCOMDateApproximated)dtx1.Value).Approximated, TGEDCOMApproximated.daEstimated);

			dtx1.ParseString("FROM 04 JAN 2013 TO 20 JAN 2013");
			Assert.IsFalse(dtx1.IsEmpty(), "!dtx1.IsEmpty");

			dtx1.ParseString("BEF 20 JAN 2013");
			Assert.AreEqual(dtx1.Date, dt, "BEF");

			dtx1.ParseString("AFT 20 JAN 2013");
			Assert.AreEqual(dtx1.Date, dt, "AFT");

			dtx1.ParseString("BET 04 JAN 2013 AND 20 JAN 2013");
			Assert.IsFalse(dtx1.IsEmpty(), "!dtx1.IsEmpty");
		}

		[Test]
		public void GEDCOMAddressTests()
		{
			TGEDCOMAddress addr = TGEDCOMAddress.Create(null, null, "ADDR", "") as TGEDCOMAddress;
			Assert.IsNotNull(addr, "addr != null");
			
			addr.aux_SetAddressValue("test");
			Assert.AreEqual(addr.Address.Text.Trim(), "test");
		}

		[Test]
		public void GEDCOMAliasTests()
		{
			TGEDCOMTag alias = TGEDCOMAlias.Create(null, null, "ALIA", "");
			Assert.IsNotNull(alias, "alias != null");
		}

		[Test]
		public void GEDCOMAssociationTests()
		{
			TGEDCOMTag association = TGEDCOMAssociation.Create(null, null, "ASSO", "");
			Assert.IsNotNull(association, "association != null");
		}

		//[Test]
		public void GEDCOMIndiTests(TGEDCOMTree tree)
		{
			
		}

		[Test]
		public void GEDCOMTreeTests()
		{
			TGEDCOMTree tree = new TGEDCOMTree();
			Assert.IsNotNull(tree, "tree != null");
			
			TGEDCOMRecord rec;
			
			//GEDCOMIndiTests(tree);
			rec = tree.AddRecord(TGEDCOMIndividualRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();

			string xref = rec.XRef;
			rec = tree.XRefIndex_Find(xref);
			Assert.IsNotNull(rec, "rec2 != null");
			Assert.AreEqual(rec.XRef, xref, "rec.XRef == xref");
			
			//
			
			rec = tree.AddRecord(TGEDCOMFamilyRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMNoteRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMSourceRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMRepositoryRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMMultimediaRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMSubmissionRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMSubmitterRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMGroupRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMResearchRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMTaskRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(TGEDCOMCommunicationRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();

			rec = tree.AddRecord(TGEDCOMLocationRecord.Create(tree, tree, "", "") as TGEDCOMRecord);
			Assert.IsNotNull(rec, "rec1 != null");
			rec.InitNew();

			tree.Pack();
		}
	}
}
