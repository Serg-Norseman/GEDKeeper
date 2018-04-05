/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKTests;
using NUnit.Framework;

namespace BSLib.SmartGraph
{
    [TestFixture]
    public class SmartGraphTests
    {
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

            Edge idg = edge;
            Assert.AreEqual(vertex, idg.Source);
            Assert.AreEqual(vertex2, idg.Target);

            Assert.AreNotEqual(0, edge.CompareTo(new Edge(vertex, vertex2, 1, null)));
            Assert.Throws(typeof(ArgumentException), () => { edge.CompareTo(null); });
            
            Vertex vert1 = edge.Source;
            Assert.AreEqual(vertex, vert1);
            Vertex vert2 = edge.Target;
            Assert.AreEqual(vertex2, vert2);
            
            using (Graph graph = new Graph())
            {
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
        public void GraphvizWriter_Tests()
        {
            using (Graph graph = new Graph())
            {
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



                string fileName = TestStubs.GetTempFilePath("test.gvf");
                string[] options = { "ratio=auto" };
                var gvw = new GraphvizWriter("testGraph", options);
                
                foreach (Vertex vtx in graph.Vertices) {
                    gvw.WriteNode(vtx.Sign, "name", "filled", "black", "box");
                }

                foreach (Edge edg in graph.Edges) {
                    gvw.WriteEdge(edg.Source.Sign, edg.Target.Sign);
                }

                gvw.SaveFile(fileName);
            }
        }
    }
}
