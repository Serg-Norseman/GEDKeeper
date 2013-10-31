using System;
using GedCom551;
using GKCore;
using NUnit.Framework;

namespace GKTests
{
	[TestFixture]
	public class CoreTests
	{
		[Test]
		public void UtilsTests()
		{
			TGEDCOMTree tree = new TGEDCOMTree();
			Assert.IsNotNull(tree);

			TGEDCOMFormat fmt = GKUtils.GetGEDCOMFormat(tree);
			Assert.AreEqual(fmt, TGEDCOMFormat.gf_Unknown);

			bool access = GKUtils.IsRecordAccess(TGEDCOMRestriction.rnConfidential, TShieldState.ssNone);
			Assert.IsTrue(access);
			
			string st = "иван";
			st = GKUtils.SetAsName(st);
			Assert.AreEqual(st, "Иван");
			
			TGEDCOMIndividualRecord iRec = GKUtils.CreatePersonEx(tree, "name", "patr", "surname", TGEDCOMSex.svMale, true);
			Assert.IsNotNull(iRec);
			Assert.AreEqual(iRec.Sex, TGEDCOMSex.svMale);

			TGEDCOMPersonalName pName = iRec.PersonalNames[0];
			string first, surname;
			pName.GetNameParts(out first, out surname);
			Assert.AreEqual(surname, "surname");
			Assert.AreEqual(first, "name patr");
			
//			TGEDCOMPersonalNamePieces pieces = pName.Pieces;
//			Assert.AreEqual(pieces.Surname, "surname");
//			Assert.AreEqual(pieces.Name, "name");
//			Assert.AreEqual(pieces.PatronymicName, "patr");
			
			string name, patr;
			iRec.aux_GetNameParts(out surname, out name, out patr);
			Assert.AreEqual(surname, "surname");
			Assert.AreEqual(name, "name");
			Assert.AreEqual(patr, "patr");
			
			TGEDCOMIndividualRecord father, mother;
			iRec.aux_GetParents(out father, out mother);
			Assert.IsNull(father);
			Assert.IsNull(mother);
			
			string dst = GKUtils.CompactDate("__.__.2013");
			Assert.AreEqual(dst, "2013");
			
			TGEDCOMFamilyRecord fRec = tree.aux_CreateFamily();
			Assert.IsNotNull(fRec);
			
			TGEDCOMCustomEvent evt = GKUtils.CreateEventEx(tree, fRec, "MARR", "28 DEC 2013", "Iv");
			Assert.IsNotNull(evt);
			
			DateTime dt = DateTime.Parse("28.12.2013");
			Assert.AreEqual(evt.Detail.Date.Date, dt, "evt.Date.Equals(dt)");

			Assert.AreEqual(evt.Detail.Place.StringValue, "Iv");
		}

		[Test]
		public void NamesTableTests()
		{
			string st;
			st = NamesTable.ClearSurname("Иванова (Петрова)");
			Assert.AreEqual(st, "Иванова");

			st = NamesTable.PrepareRusSurname("Иванова", true);
			Assert.AreEqual(st, "Иванов");
			st = NamesTable.PrepareRusSurname("Бельская", true);
			Assert.AreEqual(st, "Бельский");
			st = NamesTable.PrepareRusSurname("Грозная", true);
			Assert.AreEqual(st, "Грозный");
			st = NamesTable.PrepareRusSurname("Иванов", false);
			Assert.AreEqual(st, "Иванов");
			st = NamesTable.PrepareRusSurname("Бельский", false);
			Assert.AreEqual(st, "Бельский");
			st = NamesTable.PrepareRusSurname("Грозный", false);
			Assert.AreEqual(st, "Грозный");

			st = NamesTable.PrepareRusSurname("", false);
			Assert.AreEqual(st, "?");
			st = NamesTable.PrepareRusSurname("(Иванова)", false);
			Assert.AreEqual(st, "?");

			st = NamesTable.GetRusWifeSurname("Иванов");
			Assert.AreEqual(st, "Иванова");
			st = NamesTable.GetRusWifeSurname("Бельский");
			Assert.AreEqual(st, "Бельская");
			st = NamesTable.GetRusWifeSurname("Грозный");
			Assert.AreEqual(st, "Грозная");

			st = NamesTable.GetRusWifeSurname("");
			Assert.AreEqual(st, "?");
			
			string[] snms = NamesTable.GetSurnames("Бельская (Иванова)", true);
			Assert.AreEqual(snms.Length, 2);
			Assert.AreEqual(snms[0], "Бельский");
			Assert.AreEqual(snms[1], "Иванов");

			snms = NamesTable.GetSurnames("Бельская", true);
			Assert.AreEqual(snms.Length, 1);
			Assert.AreEqual(snms[0], "Бельский");

			snms = NamesTable.GetSurnames("Бельский", false);
			Assert.AreEqual(snms.Length, 1);
			Assert.AreEqual(snms[0], "Бельский");

			///

			TGEDCOMSex sx = NamesTable.GetSex("Мария", "Петровна", false);
			Assert.AreEqual(sx, TGEDCOMSex.svFemale);

			sx = NamesTable.GetSex("Иван", "Петрович", false);
			Assert.AreEqual(sx, TGEDCOMSex.svMale);
		}
	}
}
