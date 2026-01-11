// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using BSLib.DataViz.SmartGraph;
using GKTests;
using NUnit.Framework;

namespace BSLib
{
    [TestFixture]
    public class ExternalsTests
    {
        [Test]
        public void Test_ConvertHelper_UniformName()
        {
            string st = "ivan";
            st = StringHelper.UniformName(st);
            Assert.AreEqual("Ivan", st);

            st = StringHelper.UniformName(null);
            Assert.AreEqual(null, st);
        }

        [Test]
        public void Test_NavStack()
        {
            var navStack = new NavigationStack<object>();
            {
                Assert.IsNotNull(navStack);
                Assert.AreEqual(null, navStack.Current);
                navStack.Clear();
                Assert.AreEqual(null, navStack.Current);

                Assert.AreEqual(false, navStack.CanBackward());
                Assert.AreEqual(false, navStack.CanForward());

                object test = new object();
                object test2 = new object();

                navStack.Current = test;
                navStack.Current = test2;

                Assert.AreEqual(test, navStack.Back());
                Assert.AreEqual(test2, navStack.Next());
            }
        }

        [Test]
        public void Test_Graph()
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

            Edge idg = edge;
            Assert.AreEqual(vertex, idg.Source);
            Assert.AreEqual(vertex2, idg.Target);

            Assert.AreNotEqual(0, edge.CompareTo(new Edge(vertex, vertex2, 1, null)));
            Assert.Throws(typeof(ArgumentException), () => { edge.CompareTo(null); });
            
            Vertex vert1 = edge.Source;
            Assert.AreEqual(vertex, vert1);
            Vertex vert2 = edge.Target;
            Assert.AreEqual(vertex2, vert2);
            
            using (var graph = new Graph()) {
                Assert.IsNotNull(graph);

                /*vert1 = graph.AddVertex(null);
                Assert.IsNotNull(vert1);
                graph.DeleteVertex(vert1);*/
                
                vert1 = graph.AddVertex("test", null);
                Assert.IsNotNull(vert1);

                // the second node with the same signature is not added
                vert2 = graph.AddVertex("test", null);
                Assert.IsNotNull(vert2);
                Assert.AreEqual(vert1, vert2);

                vert2 = graph.FindVertex("test");
                Assert.AreEqual(vert1, vert2);

                graph.DeleteVertex(vert1);
                
                vert1 = graph.AddVertex("src", null);
                vert2 = graph.AddVertex("tgt", null);
                Edge edge3 = graph.AddDirectedEdge("src", "tgt", 1, null);
                Assert.IsNotNull(edge3);
                graph.DeleteEdge(edge3);
                
                edge3 = graph.AddDirectedEdge("1", "2", 1, null, false);
                Assert.IsNull(edge3);
                
                bool res = graph.AddUndirectedEdge(vert1, vert2, 1, null, null);
                Assert.AreEqual(true, res);

                graph.DeleteVertex(vert1); // "src", will be deleted subordinate edges

                foreach (Vertex vtx in graph.Vertices) {
                }

                foreach (Edge edg in graph.Edges) {
                }

                graph.Clear();

                graph.DeleteVertex(null); // no exception
                graph.DeleteEdge(null); // no exception

                graph.FindPathTree(null); // nothing will happen
            }
        }

        [Test]
        public void Test_GraphvizWriter()
        {
            using (var graph = new Graph()) {
                Vertex vert1 = graph.AddVertex("test", null);
                Assert.IsNotNull(vert1);
                
                Vertex vert2 = graph.AddVertex("test2");
                Assert.IsNotNull(vert2);
                
                vert1 = graph.AddVertex("src", null);
                vert2 = graph.AddVertex("tgt", null);
                Edge edge3 = graph.AddDirectedEdge("src", "tgt", 1, null);
                Assert.IsNotNull(edge3);
                
                edge3 = graph.AddDirectedEdge("1", "2", 1, null, false);
                Assert.IsNull(edge3);
                
                bool res = graph.AddUndirectedEdge(vert1, vert2, 1, null, null);
                Assert.AreEqual(true, res);


                string fileName = TestUtils.GetTempFilePath("test.gvf", out _);
                string[] options = { "ratio=auto" };
                var gvw = new GraphvizWriter("testGraph", options);
                
                foreach (Vertex vtx in graph.Vertices) {
                    gvw.WriteNode(vtx.Sign, "name", "filled", "black", "box");
                }

                foreach (Edge edg in graph.Edges) {
                    gvw.WriteEdge(edg.Source.Sign, edg.Target.Sign);
                }

                try {
                    gvw.SaveFile(fileName);
                } finally {
                    TestUtils.RemoveTestFile(fileName);
                }
            }
        }
    }
}
