/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Drawing;
using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCommon.SmartGraph;
using GKCore.Interfaces;
using GKCore.Types;
using GKTests.Mocks;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class CommonTests
    {
        [Test]
        public void ReflectionHelper_Tests()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.GetPropertyValue(null, "Text"); });
            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.SetPropertyValue(null, "Text", null); });
            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.GetFieldValue(null, "Text"); });
            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.SetPropertyValue(null, "Text", null); });

            using (StringList strList = new StringList()) {
                strList.Text = "Test line";

                object obj = SysUtils.GetPropertyValue(strList, "Text");
                Assert.AreEqual("Test line\r\n", obj);

                SysUtils.SetPropertyValue(strList, "Text", "Test2");
                Assert.AreEqual("Test2\r\n", strList.Text);

                Assert.Throws(typeof(ArgumentOutOfRangeException), () => { SysUtils.GetPropertyValue(strList, "test"); });
                Assert.Throws(typeof(ArgumentOutOfRangeException), () => { SysUtils.SetPropertyValue(strList, "test", ""); });
            }

            Token tkn = new Token(TokenKind.Unknown, "", 111, 0);
            object obj1 = SysUtils.GetFieldValue(tkn, "Line");
            Assert.AreEqual(111, obj1);
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { SysUtils.GetFieldValue(tkn, "Lines"); });
        }

        [Test]
        public void ConvHelper_Tests()
        {
            int ival = SysUtils.ParseInt("495", 0);
            Assert.AreEqual(495, ival);

            ival = SysUtils.ParseInt("asdfa", 11);
            Assert.AreEqual(11, ival);

            double fval = SysUtils.ParseFloat("495.575", 0);
            Assert.AreEqual(495.575, fval);

            fval = SysUtils.ParseFloat("575,495", 0, true);
            Assert.AreEqual(575.495, fval);

            fval = SysUtils.ParseFloat("", 22.1);
            Assert.AreEqual(22.1, fval);

            fval = SysUtils.ParseFloat("sdgfdf", 22.2);
            Assert.AreEqual(22.2, fval);

            string st = SysUtils.AdjustNum(9, 3);
            Assert.AreEqual("009", st);
        }

        [Test]
        public void RomeNumbers_Tests()
        {
            Assert.AreEqual("VI", SysUtils.GetRome(6), "RomeTest_00");
            Assert.AreEqual("VIII", SysUtils.GetRome(8), "RomeTest_01");
            Assert.AreEqual("IX", SysUtils.GetRome(9), "RomeTest_02");
            Assert.AreEqual("XXXI", SysUtils.GetRome(31), "RomeTest_03");
            Assert.AreEqual("XLVI", SysUtils.GetRome(46), "RomeTest_04");
            Assert.AreEqual("XCIX", SysUtils.GetRome(99), "RomeTest_05");
            Assert.AreEqual("DLXXXIII", SysUtils.GetRome(583), "RomeTest_06");
            Assert.AreEqual("DCCCLXXXVIII", SysUtils.GetRome(888), "RomeTest_07");
            Assert.AreEqual("MDCLXVIII", SysUtils.GetRome(1668), "RomeTest_08");
            Assert.AreEqual("MCMLXXXIX", SysUtils.GetRome(1989), "RomeTest_09");
            Assert.AreEqual("MMMCMXCIX", SysUtils.GetRome(3999), "RomeTest_10");
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

            string test = es.ToString().Substring(64-8);
            Assert.AreEqual("00000011", test);

            // clone test
            EnumSet<RestrictionEnum> copy = (EnumSet<RestrictionEnum>)es.Clone();
            test = copy.ToString().Substring(64-8);
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

            Assert.IsFalse(es3.ContainsAll(new RestrictionEnum[] {}));
            Assert.IsTrue(es3.ContainsAll(RestrictionEnum.rnNone, RestrictionEnum.rnLocked));
            Assert.IsFalse(es3.ContainsAll(RestrictionEnum.rnNone, RestrictionEnum.rnPrivacy));

            Assert.IsFalse(es3.HasIntersect(new RestrictionEnum[] {}));
            Assert.IsTrue(es3.HasIntersect(RestrictionEnum.rnNone, RestrictionEnum.rnPrivacy));
            Assert.IsFalse(es3.HasIntersect(RestrictionEnum.rnPrivacy));

            es = es - es2;
            es3 = EnumSet<RestrictionEnum>.Create(RestrictionEnum.rnNone);
            Assert.IsTrue(es == es3);
            Assert.AreEqual("0000000000000000000000000000000000000000000000000000000000000001", es3.ToString());
            Assert.AreNotEqual(0, es3.GetHashCode());
        }

        [Test]
        public void IndistinctMatching_Tests()
        {
            int res1, res2;

            res1 = IndistinctMatching.LevenshteinDistance("Ivanov", "");
            Assert.AreEqual(6, res1);
            res1 = IndistinctMatching.LevenshteinDistance("", "Petroff");
            Assert.AreEqual(7, res1);
            res1 = IndistinctMatching.LevenshteinDistance("Ivanov", "Ivanov");
            Assert.AreEqual(0, res1);
            res1 = IndistinctMatching.LevenshteinDistance("Ivanvo", "Ivanov");
            Assert.AreEqual(2, res1);
            res1 = IndistinctMatching.LevenshteinDistance("Petroff", "Pterov");
            Assert.AreEqual(4, res1); // permutation -fail

            res1 = IndistinctMatching.DamerauLevenshteinDistance("Ivanov", "");
            Assert.AreEqual(6, res1);
            res1 = IndistinctMatching.DamerauLevenshteinDistance("", "Petroff");
            Assert.AreEqual(7, res1);
            res2 = IndistinctMatching.DamerauLevenshteinDistance("Ivanov", "Ivanov");
            Assert.AreEqual(0, res2);
            res2 = IndistinctMatching.DamerauLevenshteinDistance("Ivanvo", "Ivanov");
            Assert.AreEqual(1, res2);
            res1 = IndistinctMatching.DamerauLevenshteinDistance("Petroff", "Pterov");
            Assert.AreEqual(3, res1); // permutation -ok

            Assert.Throws(typeof(ArgumentNullException), () => { IndistinctMatching.GetSimilarity("Ivanvo", null); });
            Assert.Throws(typeof(ArgumentNullException), () => { IndistinctMatching.GetSimilarity(null, "Ivanov"); });

            Assert.GreaterOrEqual(IndistinctMatching.GetSimilarity("Ivanov", "Ivanov"), 1.0f);
            Assert.GreaterOrEqual(IndistinctMatching.GetSimilarity("Ivanvo", "Ivanov"), 0.833f);
        }

        [Test]
        public void IndistinctMatching_PerfTest1()
        {
            for (int i = 1; i < 10000; i++) {
                IndistinctMatching.LevenshteinDistance("Ivan", "Ivanov");
                IndistinctMatching.DamerauLevenshteinDistance("Ivan", "Ivanov");
            }
        }

        [Test]
        public void IndistinctMatching_PerfTest2()
        {
            for (int i = 1; i < 10000; i++) {
                IndistinctMatching.LevenshteinDistance("Ivanvo", "Ivanov");
                IndistinctMatching.DamerauLevenshteinDistance("Ivanvo", "Ivanov");
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

            StringList strList2 = new StringList();
            strList2.Assign(null);
            strList2.Assign(strList);
            Assert.AreEqual("The", strList2[0]);
            Assert.AreEqual("string", strList2[1]);
            Assert.AreEqual("list", strList2[2]);
            Assert.AreEqual("test", strList2[3]);
            strList2.Clear();
            strList2.AddStrings(null);
            strList2.AddStrings(strList);
            Assert.AreEqual("The", strList2[0]);
            Assert.AreEqual("string", strList2[1]);
            Assert.AreEqual("list", strList2[2]);
            Assert.AreEqual("test", strList2[3]);
            Assert.Throws(typeof(StringListException), () => { strList2.Delete(-1); });
            Assert.Throws(typeof(StringListException), () => { strList2.Exchange(-1, 0); });
            Assert.Throws(typeof(StringListException), () => { strList2.Exchange(0, -1); });

            Assert.Throws(typeof(StringListException), () => { object item = strList2[-1]; });
            Assert.Throws(typeof(StringListException), () => { strList2[-1] = null; });
            Assert.Throws(typeof(StringListException), () => { object item = strList2.GetObject(-1); });
            Assert.Throws(typeof(StringListException), () => { strList2.SetObject(-1, null); });

            string[] listVals = strList.ToArray();
            Assert.AreEqual("The", listVals[0]);
            Assert.AreEqual("string", listVals[1]);
            Assert.AreEqual("list", listVals[2]);
            Assert.AreEqual("test", listVals[3]);

            strList.Exchange(1, 2);
            Assert.AreEqual("string", strList[2]);
            Assert.AreEqual("list", strList[1]);

            strList[2] = "string2";
            Assert.AreEqual("string2", strList[2]);

            object obj = new object();
            strList.SetObject(2, obj);
            Assert.AreEqual(obj, strList.GetObject(2));
            Assert.AreEqual(2, strList.IndexOfObject(obj));
            Assert.AreEqual(-1, strList.IndexOfObject(new object()));

            strList.CaseSensitive = true;
            Assert.IsTrue(strList.CaseSensitive);

            strList.DuplicateSolve = DuplicateSolve.Accept;
            Assert.AreEqual(DuplicateSolve.Accept, strList.DuplicateSolve);

            strList.DuplicateSolve = DuplicateSolve.Error;
            strList.Sorted = true;
            Assert.Throws(typeof(StringListException), () => { strList.Add("The"); });

            Assert.Throws(typeof(StringListException), () => { strList.Insert(0, "insert test"); }); // Operation not allowed on sorted list
            strList.Sorted = false;
            strList.Insert(0, "insert test");
            Assert.AreEqual("insert test", strList[0]);
            Assert.Throws(typeof(StringListException), () => { strList.Insert(-1, "insert test2"); }); // List index out of bounds

            strList.Clear();
            Assert.IsTrue(strList.IsEmpty());


            strList = new StringList(list);
            Assert.AreEqual(0, strList.IndexOf("The"));
            Assert.AreEqual(1, strList.IndexOf("string"));
            Assert.AreEqual(2, strList.IndexOf("list"));
            Assert.AreEqual(3, strList.IndexOf("test"));
            Assert.AreEqual(-1, strList.IndexOf("abrakadabra"));

            strList.DuplicateSolve = DuplicateSolve.Accept;
            strList.Add("string");
            strList.Sorted = true;
            Assert.AreEqual(0, strList.IndexOf("list"));
            Assert.AreEqual(1, strList.IndexOf("string"));
            Assert.AreEqual(3, strList.IndexOf("test"));
            Assert.AreEqual(4, strList.IndexOf("The"));
            Assert.AreEqual(-1, strList.IndexOf("abrakadabra"));
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
            Assert.AreEqual(null, edge.Value);

            IEdge idg = edge;
            Assert.AreEqual(vertex, idg.Source);
            Assert.AreEqual(vertex2, idg.Target);

            Assert.AreNotEqual(0, edge.CompareTo(new Edge(vertex, vertex2, 1, null)));
            Assert.Throws(typeof(ArgumentException), () => { edge.CompareTo(null); });
            
            IVertex vert1 = edge.Source;
            Assert.AreEqual(vertex, vert1);
            IVertex vert2 = edge.Target;
            Assert.AreEqual(vertex2, vert2);
            
            using (Graph graph = new Graph())
            {
                Assert.IsNotNull(graph);
                
                /*vert1 = graph.AddVertex(null);
				Assert.IsNotNull(vert1);
				graph.DeleteVertex(vert1);*/
                
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

                foreach (IVertex vtx in graph.Vertices) {
                }

                foreach (IEdge edg in graph.Edges) {
                }

                graph.Clear();

                graph.DeleteVertex(null); // no exception
                graph.DeleteEdge(null); // no exception
            }
        }

        private static bool GetVarEventHandler(object sender, string varName, ref double varValue)
        {
            if (varName.Equals("alpha")) {
                varValue = 15.0;
                return true;
            }

            return false;
        }

        [Test]
        public void ExpCalculator_Tests()
        {
            ExpCalculator calc = new ExpCalculator();
            Assert.IsNotNull(calc);

            calc.CaseSensitive = false;
            Assert.AreEqual(false, calc.CaseSensitive);

            calc.OnGetVar += GetVarEventHandler;

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("12+"); }); // syntax error
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("(12+"); }); // syntax error
            //Assert.Throws(typeof(CalculateException), () => { calc.Calc("5 + 0x"); }); // syntax error
            Assert.Throws(typeof(CalculateException), () => { calc.Calc(")"); }); // syntax error

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

            Assert.AreEqual(-2.0005, calc.Calc("-2.0005"));

            Assert.AreEqual(-2.0e+1, calc.Calc("-2.0e+1"));
            Assert.AreEqual(-2.0e-1, calc.Calc("-2.0e-1"));
            Assert.AreEqual(-2.0e0, calc.Calc("-2.0e0"));
            Assert.AreEqual(525.0d, calc.Calc("5.25e+2"));

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

            calc.OnGetVar -= GetVarEventHandler;

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

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("0x15j"); });
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("0b015"); });

            val = calc.Calc("if(3 == 3; 2; 3)");
            Assert.AreEqual(2, Math.Round(val, 0));

            val = calc.Calc("if(2 == 3; 2; 3)");
            Assert.AreEqual(3, Math.Round(val, 0));

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("if(2 == 3)"); }); // syntax error
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

            rt = rt.GetOffset(10, 10);
            Assert.AreEqual("{X=15,Y=13,Width=4,Height=14}", rt.ToString());
            
            Assert.IsTrue(rt.IntersectsWith(ExtRect.Create(16, 14, 20, 20)));
            
            rt = ExtRect.CreateEmpty();
            Assert.IsTrue(rt.IsEmpty());

            Assert.IsFalse(rt.Contains(5, 5));
            
            Rectangle rect = rt.ToRectangle();
            Assert.AreEqual(0, rect.Left);
            Assert.AreEqual(0, rect.Top);
            Assert.AreEqual(0, rect.Right);
            Assert.AreEqual(0, rect.Bottom);
        }

        [Test]
        public void ExtList_Tests()
        {
            using (ExtList<object> list = new ExtList<object>(true))
            {
                Assert.IsNotNull(list);
            }

            using (ExtList<object> list = new ExtList<object>())
            {
                Assert.IsNotNull(list);
                Assert.AreEqual(0, list.Count);

                Assert.Throws(typeof(ListException), () => { list[-1] = null; });

                object obj = new object();
                list.Add(obj);
                Assert.AreEqual(1, list.Count);
                Assert.AreEqual(obj, list[0]);
                Assert.AreEqual(0, list.IndexOf(obj));

                list.Delete(0);
                Assert.AreEqual(0, list.Count);

                list.Add(obj);
                Assert.AreEqual(obj, list.Extract(obj));

                list.Insert(0, obj);

                list[0] = obj;
                Assert.AreEqual(obj, list[0]);

                list.Add(null);
                Assert.AreEqual(2, list.Count);
                list.Pack();
                Assert.AreEqual(1, list.Count);

                list.Remove(obj);
                Assert.AreEqual(0, list.Count);

                Assert.AreEqual(false, list.OwnsObjects);

                list.OwnsObjects = true;
                Assert.AreEqual(true, list.OwnsObjects);

                object obj1= new object();
                list.Clear();
                list.Add(obj);
                list.Add(obj1);
                Assert.AreEqual(obj, list[0]);
                Assert.AreEqual(obj1, list[1]);
                list.Exchange(0, 1);
                Assert.AreEqual(obj, list[1]);
                Assert.AreEqual(obj1, list[0]);
            }

            using (ExtList<ValItem> list = new ExtList<ValItem>())
            {
                Assert.IsNotNull(list);

                list.Add(new ValItem(5));
                list.Add(new ValItem(1));
                list.Add(new ValItem(17));
                list.Add(new ValItem(4));

                list.QuickSort(CompareItems);

                list.MergeSort(CompareItems);
            }
        }

        private int CompareItems(ValItem item1, ValItem item2)
        {
            return item1.Value.CompareTo(item2.Value);
        }

        [Test]
        public void Ranges_Tests()
        {
            Assert.Throws(typeof(ArgumentException), () => { new Range<int>((2), (1)); });

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

        [Test]
        public void PG_Tests()
        {
            PatriarchObj pObj = new PatriarchObj();
            Assert.IsNotNull(pObj);
            Assert.IsNotNull(pObj.Links);

            PGNode pgNode = new PGNode("label", PGNodeType.Default);
            Assert.IsNotNull(pgNode);

            pgNode = new PGNode("label", PGNodeType.Default, 5);
            Assert.IsNotNull(pgNode);
        }

        private bool TestExternalFilterHandler(GEDCOMRecord record)
        {
            return false;
        }

        [Test]
        public void FiltersIntf_Tests()
        {
            FilterCondition cond = new FilterCondition(0, ConditionKind.ck_Contains, null);
            Assert.IsNotNull(cond);

            ExternalFilterHandler handler = this.TestExternalFilterHandler;
            Assert.IsFalse(handler.Invoke(null));
        }

        [Test]
        public void StringTokenizer_Tests()
        {
            //Assert.Throws(typeof(ArgumentNullException), () => { new StringTokenizer(null); });

            StringTokenizer strTok = new StringTokenizer("alpha beta 123  456.57, x");
            Assert.IsNotNull(strTok);

            strTok.IgnoreWhiteSpace = false;
            Assert.IsFalse(strTok.IgnoreWhiteSpace);

            char[] symChars = strTok.SymbolChars;
            strTok.SymbolChars = symChars;

            strTok.RecognizeDecimals = false;
            Assert.IsFalse(strTok.RecognizeDecimals);

            Token tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("alpha", tok.Value);
            Assert.AreEqual(5, strTok.Position);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("beta", tok.Value);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("123", tok.Value);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("456", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Symbol, tok.Kind);
            Assert.AreEqual(".", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("57", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Symbol, tok.Kind);
            Assert.AreEqual(",", tok.Value);

            Assert.AreEqual(TokenKind.WhiteSpace, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("x", tok.Value);

            //

            strTok = new StringTokenizer("alpha beta 123 456.57, x; \r \n \"test quote\"");
            Assert.IsNotNull(strTok);

            strTok.IgnoreWhiteSpace = true;
            Assert.IsTrue(strTok.IgnoreWhiteSpace);

            strTok.RecognizeDecimals = true;
            Assert.IsTrue(strTok.RecognizeDecimals);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("alpha", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("beta", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("123", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Number, tok.Kind);
            Assert.AreEqual("456.57", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Symbol, tok.Kind);
            Assert.AreEqual(",", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Word, tok.Kind);
            Assert.AreEqual("x", tok.Value);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.Symbol, tok.Kind);
            Assert.AreEqual(";", tok.Value);

            Assert.AreEqual(TokenKind.EOL, strTok.Next().Kind);
            Assert.AreEqual(TokenKind.EOL, strTok.Next().Kind);

            tok = strTok.Next();
            Assert.AreEqual(TokenKind.QuotedString, tok.Kind);
            Assert.AreEqual("\"test quote\"", tok.Value);
        }

        [Test]
        public void ValuesCollection_Tests()
        {
            ValuesCollection valsCol = new ValuesCollection();

            valsCol.Add("red", "rojo");
            valsCol.Add("green", "verde");
            valsCol.Add("blue", "azul", true);
            valsCol.Add("red", "rouge");
            valsCol.Add("red", null);
            valsCol.Add("red", "rouge", true);

            Assert.AreEqual(3, valsCol.Count);

            valsCol.Remove("green");
            Assert.AreEqual(2, valsCol.Count);

            string[] values = valsCol.GetValues("xxxxx");
            Assert.AreEqual(null, values);

            values = valsCol.GetValues("red");
            Assert.AreEqual("rojo", values[0]);
            Assert.AreEqual("rouge", values[1]);

            valsCol.Clear();
            Assert.AreEqual(0, valsCol.Count);
        }

        [Test]
        public void SysUtils_Tests()
        {
            #if __MonoCS__
            Assert.IsTrue(SysUtils.IsUnix());
            Assert.AreEqual(PlatformID.Unix, SysUtils.GetPlatformID());
            #else
            Assert.IsFalse(SysUtils.IsUnix());
            Assert.AreEqual(PlatformID.Win32NT, SysUtils.GetPlatformID());
            #endif

            //

            uint days = SysUtils.DaysBetween(new DateTime(1990, 10, 10), new DateTime(1990, 10, 13));
            Assert.AreEqual(3, days);

            Assert.AreEqual(31, SysUtils.DaysInAMonth(1990, 5));

            //

            Assert.AreEqual(true, SysUtils.IsSetBit(3, 0));
            Assert.AreEqual(true, SysUtils.IsSetBit(3, 1));
            Assert.AreEqual(false, SysUtils.IsSetBit(3, 4));

            //

            Assert.AreEqual(495, SysUtils.Trunc(495.575));

            Assert.AreEqual(3.0f, SysUtils.SafeDiv(9.0f, 3.0f));
            Assert.AreEqual(0.0f, SysUtils.SafeDiv(9.0f, 0.0f));

            //

            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.FirstOrDefault<int>(null); });
            int N = SysUtils.FirstOrDefault<int>(new int[] { 5, 7, 10 });
            Assert.AreEqual(5, N);

            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.LastOrDefault<int>(null); });
            N = SysUtils.LastOrDefault<int>(new int[] { 5, 7, 10 });
            Assert.AreEqual(10, N);

            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.SingleOrDefault<int>(null); });
            N = SysUtils.SingleOrDefault<int>(new int[] { 11 });
            Assert.AreEqual(11, N);
            N = SysUtils.SingleOrDefault<int>(new int[] { });
            Assert.AreEqual(0, N);
            Assert.Throws(typeof(Exception), () => { SysUtils.SingleOrDefault<int>(new int[] { 5, 7, 10 }); });
        }

        private void TweenHandler(int newX, int newY)
        {
        }

        [Test]
        public void Tween_Tests()
        {
            #if !__MonoCS__
            TweenLibrary tween = new TweenLibrary();
            tween.StartTween(TweenHandler, 0, 0, 10, 10, TweenAnimation.EaseInOutQuad, 20);
            #endif
        }

        [Test]
        public void GfxHelper_Tests()
        {
            Assert.AreEqual(57.295779513, SysUtils.RadiansToDegrees(1.0), 0.0000000001);
            Assert.AreEqual(1.0, SysUtils.DegreesToRadians(57.295779513), 0.0000000001);

            Assert.AreEqual(2.0, SysUtils.ZoomToFit(50, 20, 100, 50));
            Assert.AreEqual(3.0, SysUtils.ZoomToFit(15, 40, 45, 120));

            Assert.AreEqual(Color.FromArgb(50, 50, 50), SysUtils.Darker(Color.FromArgb(100, 100, 100), 0.5f));
            Assert.AreEqual(Color.FromArgb(75, 75, 75), SysUtils.Lighter(Color.FromArgb(50, 50, 50), 0.5f));
        }


        [Test]
        public void ListItems_Tests()
        {
            var item1 = new GKListItem(10, null);
            var item2 = new GKListItem(20, null);
            Assert.AreEqual(-1, item1.CompareTo(item2));

            var subitem1 = new GKListSubItem(10);
            var subitem2 = new GKListSubItem(20);
            Assert.AreEqual(-1, subitem1.CompareTo(subitem2));
        }
    }
}
