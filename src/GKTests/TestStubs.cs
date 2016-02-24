using GKCommon.GEDCOM;
using GKCore;

using NUnit.Framework;

namespace GKTests
{
	[SetUpFixture]
	public class TestStubs
	{
		public TestStubs()
		{
		}

		[SetUp]
		public void Setup()
		{
			//Console.WriteLine("=====START=====");
		}

		[TearDown]
		public void TearDown()
		{
			//Console.WriteLine("=====BYE!======");
		}
		
		
		public static BaseContext CreateContext()
		{
			GEDCOMTree tree = new GEDCOMTree();
			Assert.IsNotNull(tree);

			BaseContext context = new BaseContext(tree, null);
			
			return context;
		}
		
		public static void FillContext(BaseContext context)
		{
			GEDCOMIndividualRecord iRec = context.CreatePersonEx("Ivan", "Ivanovich", "Ivanov", GEDCOMSex.svMale, true);
			Assert.IsNotNull(iRec);
			
			GEDCOMCustomEvent evt = context.CreateEventEx(null, "BIRT", "xxxxx", "xxxxx");
			Assert.IsNull(evt);
			
			evt = iRec.FindEvent("BIRT");
			Assert.IsNotNull(evt);
			evt.Detail.Date.ParseString("28 DEC 1990");
			evt.Detail.Place.StringValue = "Ivanovo";
			
			GEDCOMCustomEvent evtd = context.CreateEventEx(iRec, "DEAT", "28 DEC 2010", "Ivanovo");
			Assert.IsNotNull(evtd);

			//
			
			GEDCOMIndividualRecord iRec2 = context.CreatePersonEx("Maria", "Ivanovna", "Ivanova", GEDCOMSex.svFemale, true);
			evt = iRec2.FindEvent("BIRT");
			Assert.IsNotNull(evt);
			evt.Detail.Date.ParseString("17 MAR 1990");
			evt.Detail.Place.StringValue = "Ivanovo";
			
			//
			
			GEDCOMFamilyRecord famRec = context.Tree.CreateFamily();
			Assert.IsNotNull(famRec);
			famRec.AddSpouse(iRec);
			famRec.AddSpouse(iRec2);

			//
			
			GEDCOMIndividualRecord iRec3 = context.CreatePersonEx("Maria", "Ivanovna", "Ivanova", GEDCOMSex.svFemale, true);
			evt = iRec3.FindEvent("BIRT");
			Assert.IsNotNull(evt);
			evt.Detail.Date.ParseString("17 MAR 1990");
			evt.Detail.Place.StringValue = "Ivanovo";

			//
			
			GEDCOMGroupRecord groupRec = context.Tree.CreateGroup();
			Assert.IsNotNull(groupRec, "group1 != null");
		}
	}
}
