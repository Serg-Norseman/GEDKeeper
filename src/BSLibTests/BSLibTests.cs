using System;
using System.Drawing;

using BSLib;
using BSLib.SmartGraph;
using NUnit.Framework;

namespace BSLibTests
{
	[TestFixture]
	public class BSLibTests
	{
		[Test]
		public void ConvHelper_Tests()
		{
			int ival = ConvHelper.ParseInt("495", 0);
			Assert.AreEqual(ival, 495);

			double fval = ConvHelper.ParseFloat("495.575", 0);
			Assert.AreEqual(fval, 495.575);

			string st = ConvHelper.AdjustNum(9, 3);
			Assert.AreEqual(st, "009");
		}

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

		private enum RestrictionEnum
		{
			rnNone,
			rnLocked,
			rnConfidential,
			rnPrivacy,

			rnLast = rnPrivacy
		}

		[Test]
		public void EnumSet_Tests()
		{
			EnumSet<RestrictionEnum> es = EnumSet<RestrictionEnum>.Create();
			Assert.IsTrue(es.IsEmpty());

			es.Include(null);
			Assert.IsTrue(es.IsEmpty());
			
			es.Include(RestrictionEnum.rnPrivacy, RestrictionEnum.rnLocked);
			Assert.IsTrue(es.Contains(RestrictionEnum.rnPrivacy));
			Assert.IsFalse(es.Contains(RestrictionEnum.rnNone));
			Assert.IsFalse(es.IsEmpty());

			es.Exclude(RestrictionEnum.rnPrivacy);
			Assert.IsFalse(es.Contains(RestrictionEnum.rnPrivacy));
			Assert.IsTrue(es.Contains(RestrictionEnum.rnLocked));
			
			es = EnumSet<RestrictionEnum>.Create(RestrictionEnum.rnNone, RestrictionEnum.rnLocked);
			Assert.IsTrue(es.Contains(RestrictionEnum.rnNone));
			Assert.IsTrue(es.Contains(RestrictionEnum.rnLocked));
			
			string test = es.ByteToStr(0);
			Assert.AreEqual("00000011", test);
			
			// clone test
			EnumSet<RestrictionEnum> copy = (EnumSet<RestrictionEnum>)es.Clone();
			test = copy.ByteToStr(0);
			Assert.AreEqual("00000011", test);
			
			// clear test
			copy.Clear();
			Assert.IsTrue(copy.IsEmpty());
			
			//
			EnumSet<RestrictionEnum> es2 = EnumSet<RestrictionEnum>.Create(RestrictionEnum.rnNone, RestrictionEnum.rnLocked);

			Assert.IsTrue(es.Equals(es2));
			Assert.IsFalse(es.Equals(null));
			
			Assert.IsTrue(es.Contains(RestrictionEnum.rnLocked));
			Assert.IsFalse(es.Contains(RestrictionEnum.rnPrivacy));

			EnumSet<RestrictionEnum> es3 = EnumSet<RestrictionEnum>.Create(RestrictionEnum.rnLocked);
			EnumSet<RestrictionEnum> es4 = es * es3;
            Assert.IsTrue(es4.Contains(RestrictionEnum.rnLocked));
			
			es = EnumSet<RestrictionEnum>.Create(RestrictionEnum.rnNone);
			es2 = EnumSet<RestrictionEnum>.Create(RestrictionEnum.rnLocked);
			Assert.IsTrue(es != es2);
			
			es = es + es2;
			es3 = EnumSet<RestrictionEnum>.Create(RestrictionEnum.rnNone, RestrictionEnum.rnLocked);
			Assert.IsTrue(es.Equals(es3));
			
			Assert.IsTrue(es3.ContainsAll(RestrictionEnum.rnNone, RestrictionEnum.rnLocked));
			Assert.IsFalse(es3.ContainsAll(RestrictionEnum.rnNone, RestrictionEnum.rnPrivacy));
			Assert.IsTrue(es3.HasIntersect(RestrictionEnum.rnNone, RestrictionEnum.rnPrivacy));
			Assert.IsFalse(es3.HasIntersect(RestrictionEnum.rnPrivacy));
			
			es = es - es2;
			es3 = EnumSet<RestrictionEnum>.Create(RestrictionEnum.rnNone);
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
			string[] list = new string[] { "The", "string", "list", "test" };
			
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

		[Test]
		public void TList_Tests()
		{
			
		}

		[Test]
		public void Ranges_Tests()
		{
			Assert.IsTrue(new Range<int>((1), (2)).IsOverlapped(new Range<int>((1), (2))), "chk1"); // true
			Assert.IsTrue(new Range<int>((1), (3)).IsOverlapped(new Range<int>((2), (4))), "chk2"); // true
			Assert.IsTrue(new Range<int>((2), (4)).IsOverlapped(new Range<int>((1), (3))), "chk3"); // true
			Assert.IsFalse(new Range<int>((3), (4)).IsOverlapped(new Range<int>((1), (2))), "chk4"); // false
			Assert.IsFalse(new Range<int>((1), (2)).IsOverlapped(new Range<int>((3), (4))), "chk5"); // false
            Assert.IsTrue(new Range<int>((2), (3)).IsOverlapped(new Range<int>((1), (4))), "chk6"); // true
            Assert.IsTrue(new Range<int>((1), (4)).IsOverlapped(new Range<int>((2), (3))), "chk7"); // true

            Assert.IsTrue(new Range<int>((1), (2)).IsOverlapped(new Range<int>((1), (4))), "chk8"); // true
            Assert.IsTrue(new Range<int>((1), (4)).IsOverlapped(new Range<int>((1), (2))), "chk9"); // true
            Assert.IsTrue(new Range<int>((1), (4)).IsOverlapped(new Range<int>((3), (4))), "chk10"); // true
            Assert.IsTrue(new Range<int>((3), (4)).IsOverlapped(new Range<int>((1), (4))), "chk11"); // true
		}

	}
}