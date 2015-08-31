using System;
using System.IO;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Types;
using NUnit.Framework;

namespace GKTests
{
	[TestFixture]
	public class CoreTests
	{
		private void PersonalName_Tests(GEDCOMIndividualRecord iRec)
		{
			GEDCOMPersonalName pName = iRec.PersonalNames[0];
			string first, surname;
			pName.GetNameParts(out first, out surname);
			Assert.AreEqual("surname", surname);
			Assert.AreEqual("name patr", first);
			
//			GEDCOMPersonalNamePieces pieces = pName.Pieces;
//			Assert.AreEqual(pieces.Surname, "surname");
//			Assert.AreEqual(pieces.Name, "name");
//			Assert.AreEqual(pieces.PatronymicName, "patr");
			
			string name, patr;
			iRec.GetNameParts(out surname, out name, out patr);
			Assert.AreEqual("surname", surname);
			Assert.AreEqual("name", name);
			Assert.AreEqual("patr", patr);
		}
		
		[Test]
		public void Utils_Tests()
		{
			GEDCOMTree tree = new GEDCOMTree();
			Assert.IsNotNull(tree);

			BaseContext context = new BaseContext(tree, null);
			
			// createPerson test
			/*try
			{
				context.CreatePersonEx(null, "name", "patr", "surname", GEDCOMSex.svMale, true);
				Assert.Fail("CreatePersonEx(null) must raise exception");
			}
			catch (ArgumentNullException)
			{
				Debug.WriteLine("GKUtils.CreatePersonEx(null) is success");
			}
			catch (Exception)
			{
				throw;
			}*/
			
			// createPerson test
			GEDCOMIndividualRecord iRec = context.CreatePersonEx("name", "patr", "surname", GEDCOMSex.svMale, true);
			Assert.IsNotNull(iRec);
			Assert.AreEqual(GEDCOMSex.svMale, iRec.Sex);

			this.PersonalName_Tests(iRec);
						
			//
			
			GEDCOMCustomEvent evt = context.CreateEventEx(null, "BIRT", "28 DEC 1990", "Ivanovo");
			Assert.IsNull(evt);
			
			GEDCOMNoteRecord note = tree.aux_CreateNote();
			evt = context.CreateEventEx(note, "BIRT", "28 DEC 1990", "Ivanovo");
			Assert.IsNull(evt);
			
			//
			
			evt = context.CreateEventEx(iRec, "BIRT", "28 DEC 1990", "Ivanovo");
			Assert.IsNotNull(evt);
			GEDCOMCustomEventTest(evt, "28.12.1990");
			
			string ds = GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, DateFormat.dfDD_MM_YYYY, false);
			Assert.AreEqual("28.12.1990", ds);
			
			evt = context.CreateEventEx(iRec, "FACT", "17 JAN 2013", "Ivanovo");
			Assert.IsNotNull(evt);
			GEDCOMCustomEventTest(evt, "17.01.2013");
			
			GEDCOMIndividualRecord father, mother;
			iRec.GetParents(out father, out mother);
			Assert.IsNull(father);
			Assert.IsNull(mother);
			
			string dst = GKUtils.CompactDate("__.__.2013");
			Assert.AreEqual("2013", dst);
			
			GEDCOMFamilyRecord fRec = tree.aux_CreateFamily();
			Assert.IsNotNull(fRec);
			
			evt = context.CreateEventEx(fRec, "MARR", "28 DEC 2013", "Ivanovo");
			Assert.IsNotNull(evt);
			GEDCOMCustomEventTest(evt, "28.12.2013");
			
			// format tests
			GEDCOMFormat fmt = GKUtils.GetGEDCOMFormat(null);
			Assert.AreEqual(GEDCOMFormat.gf_Unknown, fmt);

			fmt = GKUtils.GetGEDCOMFormat(tree);
			Assert.AreEqual(GEDCOMFormat.gf_Unknown, fmt);
			
			// other
			string st = "иван";
			st = GEDCOMUtils.NormalizeName(st);
			Assert.AreEqual("Иван", st);
			
			st = GEDCOMUtils.NormalizeName(null);
			Assert.AreEqual("", st);
			
			// sex tests
			GEDCOMSex sex;
			sex = GKUtils.GetSexBySign('F');
			Assert.AreEqual(GEDCOMSex.svFemale, sex);
			sex = GKUtils.GetSexBySign('M');
			Assert.AreEqual(GEDCOMSex.svMale, sex);
			sex = GKUtils.GetSexBySign('U');
			Assert.AreEqual(GEDCOMSex.svUndetermined, sex);
			
			// path tests
			st = GKUtils.GetTempDir();
			Assert.IsTrue(Directory.Exists(st));
			
			st = GKUtils.GetAppPath();
			Assert.IsTrue(Directory.Exists(st));
			
			// matches tests
			bool res = GKUtils.MatchesMask("abrakadabra", "*kad*");
			Assert.IsTrue(res);
			
			res = GKUtils.MatchesMask("abrakadabra", "*test*");
			Assert.IsFalse(res);

			// access tests
			Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.ssNone));
			Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.ssNone));
			Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.ssNone));

			Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.ssMiddle));
			Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.ssMiddle));
			Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.ssMiddle));

			Assert.IsTrue(GKUtils.IsRecordAccess(GEDCOMRestriction.rnNone, ShieldState.ssMaximum));
			Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnConfidential, ShieldState.ssMaximum));
			Assert.IsFalse(GKUtils.IsRecordAccess(GEDCOMRestriction.rnPrivacy, ShieldState.ssMaximum));
		}

		private void GEDCOMCustomEventTest(GEDCOMCustomEvent evt, string dateTest)
		{
			GEDCOMEventDetailTest(evt.Detail, dateTest);
		}
		
		private void GEDCOMPlaceTest(GEDCOMPlace place)
		{
			place.Form = "abrakadabra";
			Assert.AreEqual("abrakadabra", place.Form);
			
			GedcomTests.GEDCOMMapTest(place.Map);			
		}

		private void GEDCOMEventDetailTest(GEDCOMEventDetail detail, string dateTest)
		{
			Assert.AreEqual(DateTime.Parse(dateTest), detail.Date.Date);
			Assert.AreEqual("Ivanovo", detail.Place.StringValue);
			
			GEDCOMPlaceTest(detail.Place);
			
			detail.Agency = "test agency";
			Assert.AreEqual("test agency", detail.Agency);
			
			detail.Classification = "test type";
			Assert.AreEqual("test type", detail.Classification);
			
			detail.Cause = "test cause";
			Assert.AreEqual("test cause", detail.Cause);
			
			detail.ReligiousAffilation = "test aff";
			Assert.AreEqual("test aff", detail.ReligiousAffilation);
			
			detail.Restriction = GEDCOMRestriction.rnLocked;
			Assert.AreEqual(GEDCOMRestriction.rnLocked, detail.Restriction);
			
			GedcomTests.GEDCOMAddressTest(detail.Address, false);
		}

		[Test]
		public void NamesTable_Tests()
		{
			string st;
			st = NamesTable.ClearSurname(null);
			Assert.AreEqual("", st);

			st = NamesTable.ClearSurname("");
			Assert.AreEqual("", st);

			st = NamesTable.ClearSurname("Иванова (Петрова)");
			Assert.AreEqual("Иванова", st);

			st = NamesTable.PrepareRusSurname("Иванова", true);
			Assert.AreEqual("Иванов", st);
			st = NamesTable.PrepareRusSurname("Бельская", true);
			Assert.AreEqual("Бельский", st);
			st = NamesTable.PrepareRusSurname("Грозная", true);
			Assert.AreEqual("Грозный", st);
			st = NamesTable.PrepareRusSurname("Иванов", false);
			Assert.AreEqual("Иванов", st);
			st = NamesTable.PrepareRusSurname("Бельский", false);
			Assert.AreEqual("Бельский", st);
			st = NamesTable.PrepareRusSurname("Грозный", false);
			Assert.AreEqual("Грозный", st);

			st = NamesTable.PrepareRusSurname(null, false);
			Assert.AreEqual("?", st);
			st = NamesTable.PrepareRusSurname("", false);
			Assert.AreEqual("?", st);
			st = NamesTable.PrepareRusSurname("(Иванова)", false);
			Assert.AreEqual("?", st);

			st = NamesTable.GetRusWifeSurname("Иванов");
			Assert.AreEqual("Иванова", st);
			st = NamesTable.GetRusWifeSurname("Бельский");
			Assert.AreEqual("Бельская", st);
			st = NamesTable.GetRusWifeSurname("Грозный");
			Assert.AreEqual("Грозная", st);

			st = NamesTable.GetRusWifeSurname("");
			Assert.AreEqual("?", st);
			st = NamesTable.GetRusWifeSurname(null);
			Assert.AreEqual("?", st);
			
			string[] snms = NamesTable.GetSurnames("Бельская (Иванова)", true);
			Assert.AreEqual(2, snms.Length);
			Assert.AreEqual("Бельский", snms[0]);
			Assert.AreEqual("Иванов", snms[1]);

			snms = NamesTable.GetSurnames("Бельская", true);
			Assert.AreEqual(1, snms.Length);
			Assert.AreEqual("Бельский", snms[0]);

			snms = NamesTable.GetSurnames("Бельский", false);
			Assert.AreEqual(1, snms.Length);
			Assert.AreEqual("Бельский", snms[0]);

			///

			GEDCOMSex sx = NamesTable.GetSex("Мария", "Петровна", false);
			Assert.AreEqual(GEDCOMSex.svFemale, sx);

			sx = NamesTable.GetSex("Иван", "Петрович", false);
			Assert.AreEqual(GEDCOMSex.svMale, sx);
		}
	}
}
