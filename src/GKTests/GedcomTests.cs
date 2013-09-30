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
			Assert.True(st == "abcd");
			Assert.True(s2 == " 12345 efgh");
			
			s2 = GEDCOMUtils.ExtractDelimiter(s2, 0);
			Assert.True(s2 == "12345 efgh");
			
			int N;
			s2 = GEDCOMUtils.ExtractNumber(s2, out N, true, 0);
			Assert.True(s2 == " efgh");
			Assert.True(N == 12345);

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
			Assert.IsFalse(f == null, "f != null");
			
			f.RegisterTag("DATE", TGEDCOMDateValue.Create);

			TGEDCOMTag tag = f.CreateTag(null, null, "DATE", "");
			Assert.IsTrue(tag != null, "tag != null");

			tag = f.CreateTag(null, null, "TEST", "");
			Assert.IsTrue(tag == null, "tag == null");
		}

		[Test]
		public void GEDCOMDateTests1()
		{
			TGEDCOMChangeDate cd = new TGEDCOMChangeDate(null, null, "CHAN", "");
			Assert.IsTrue(cd != null, "cd != null");

			DateTime dtNow = DateTime.Now;
			dtNow = dtNow.AddMilliseconds(-(double)dtNow.Millisecond);
			cd.ChangeDateTime = dtNow;
			Assert.IsTrue(cd.ChangeDateTime.Equals(dtNow), "cd.ChangeDateTime == dtNow");
		}

		[Test]
		public void GEDCOMDateTests2()
		{
			TGEDCOMDateExact dtx1 = new TGEDCOMDateExact(null, null, "DATE", "20 JAN 2013");
			Assert.IsTrue(dtx1 != null, "dtx1 != null");

			DateTime dt = DateTime.Parse("20.01.2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
		}

		[Test]
		public void GEDCOMDateTests3()
		{
			TGEDCOMDateValue dtx1 = new TGEDCOMDateValue(null, null, "DATE", "20 JAN 2013");
			Assert.IsTrue(dtx1 != null, "dtx1 != null");

			DateTime dt = DateTime.Parse("20.01.2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
			
			dtx1.ParseString("INT 20 JAN 2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
			
			string st;
			
			st = "ABT 20 JAN 2013";
			dtx1.ParseString(st);
			Assert.IsTrue(dtx1.Date.Equals(dt));
			Assert.IsTrue(st == dtx1.StringValue);
			Assert.IsTrue(((TGEDCOMDateApproximated)dtx1.Value).Approximated == TGEDCOMApproximated.daAbout);
			
			st = "CAL 20 JAN 2013";
			dtx1.ParseString(st);
			Assert.IsTrue(dtx1.Date.Equals(dt));
			Assert.IsTrue(st == dtx1.StringValue);
			Assert.IsTrue(((TGEDCOMDateApproximated)dtx1.Value).Approximated == TGEDCOMApproximated.daCalculated);
			
			st = "EST 20 JAN 2013";
			dtx1.ParseString(st);
			Assert.IsTrue(dtx1.Date.Equals(dt));
			Assert.IsTrue(st == dtx1.StringValue);
			Assert.IsTrue(((TGEDCOMDateApproximated)dtx1.Value).Approximated == TGEDCOMApproximated.daEstimated);

			dtx1.ParseString("FROM 04 JAN 2013 TO 20 JAN 2013");
			Assert.IsFalse(dtx1.IsEmpty(), "!dtx1.IsEmpty");

			dtx1.ParseString("BEF 20 JAN 2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "BEF");

			dtx1.ParseString("AFT 20 JAN 2013");
			Assert.IsTrue(dtx1.Date.Equals(dt), "AFT");

			dtx1.ParseString("BET 04 JAN 2013 AND 20 JAN 2013");
			Assert.IsFalse(dtx1.IsEmpty(), "!dtx1.IsEmpty");
		}

		[Test]
		public void GEDCOMAddressTests()
		{
			TGEDCOMTag addr = TGEDCOMAddress.Create(null, null, "ADDR", "");
			Assert.IsTrue(addr != null, "addr != null");
		}

		[Test]
		public void GEDCOMAliasTests()
		{
			TGEDCOMTag alias = TGEDCOMAlias.Create(null, null, "ALIA", "");
			Assert.IsTrue(alias != null, "alias != null");
		}

		[Test]
		public void GEDCOMAssociationTests()
		{
			TGEDCOMTag association = TGEDCOMAssociation.Create(null, null, "ASSO", "");
			Assert.IsTrue(association != null, "association != null");
		}

		[Test]
		public void GEDCOMTreeTests()
		{
			TGEDCOMTree tree = new TGEDCOMTree();
			Assert.IsTrue(tree != null, "tree != null");
			
			TGEDCOMRecord rec;
			
			rec = tree.AddRecord(new TGEDCOMIndividualRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();

			string xref = rec.XRef;
			rec = tree.XRefIndex_Find(xref);
			Assert.IsTrue(rec != null, "rec2 != null");
			Assert.IsTrue(rec.XRef == xref, "rec.XRef == xref");
			
			//
			
			rec = tree.AddRecord(new TGEDCOMFamilyRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMNoteRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMSourceRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMRepositoryRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMMultimediaRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMSubmissionRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMSubmitterRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMGroupRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMResearchRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMTaskRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();
			
			rec = tree.AddRecord(new TGEDCOMCommunicationRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();

			rec = tree.AddRecord(new TGEDCOMLocationRecord(tree, tree, "", ""));
			Assert.IsTrue(rec != null, "rec1 != null");
			rec.InitNew();

			tree.Pack();
		}
	}
}
