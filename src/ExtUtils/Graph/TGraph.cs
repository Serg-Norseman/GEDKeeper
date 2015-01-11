using System;
using System.Collections.Generic;
//using System.Linq;

/// <summary>
/// 
/// </summary>

namespace ExtUtils.Graph
{
	public class TGraph : BaseObject, IGraph
	{
		#region Private members

		private sealed class DefaultDataProvider : IDataProvider
		{
			public Vertex CreateVertex()
			{
				return new Vertex();
			}

			IVertex IDataProvider.CreateVertex()
			{
				return this.CreateVertex();
			}

			public Edge CreateEdge(Vertex u, Vertex v, int cost, object value)
			{
				return new Edge(u, v, cost, value);
			}

			IEdge IDataProvider.CreateEdge(IVertex u, IVertex v, int cost, object value)
			{
				return this.CreateEdge((Vertex)u, (Vertex)v, cost, value);
			}
		}

		private class PathCandidate
		{
			public IVertex Node { get; private set; }
			public PathCandidate Next { get; private set; }
			
			public PathCandidate(IVertex node, PathCandidate next)
			{
				this.Node = node;
				this.Next = next;
			}
		}

		private readonly IDataProvider fProvider;
		private readonly List<IEdge> fEdgesList;
		private readonly List<IVertex> fVerticesList;
		private readonly Dictionary<string, IVertex> fVerticesDictionary;

		#endregion

		#region Properties

		IEnumerable<IVertex> IGraph.Vertices
		{
			get {
				return this.fVerticesList;
			}
		}

		IEnumerable<IEdge> IGraph.Edges
		{
			get {
				return this.fEdgesList;
			}
		}

		#endregion

		#region Instance control

		public TGraph() : this(new DefaultDataProvider())
		{
		}

		public TGraph(IDataProvider provider)
		{
			this.fProvider = provider;
			this.fVerticesList = new List<IVertex>();
			this.fEdgesList = new List<IEdge>();
			this.fVerticesDictionary = new Dictionary<string, IVertex>();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.Clear();
			}
			base.Dispose(disposing);
		}

		#endregion

		#region Data management

		public void Clear()
		{
			foreach (IVertex vertex in this.fVerticesList) {
				vertex.EdgeIn = null;
				vertex.EdgesOut.Clear();
			}

			this.fEdgesList.Clear();
			this.fVerticesList.Clear();
			this.fVerticesDictionary.Clear();
		}

		public IVertex AddVertex(object data)
		{
			IVertex result = this.fProvider.CreateVertex();
			result.Value = data;
			this.fVerticesList.Add(result);

			return result;
		}

		public IVertex AddVertex(string sign, object data)
		{
			IVertex result = this.AddVertex(data);
			result.Sign = sign;
			this.fVerticesDictionary.Add(sign, result);

			return result;
		}

		public void AddUndirectedEdge(IVertex source, IVertex target, int cost, object srcValue, object tgtValue)
		{
			this.AddDirectedEdge(source, target, cost, srcValue);
			this.AddDirectedEdge(target, source, cost, tgtValue);
		}

		public IEdge AddDirectedEdge(string sourceSign, string targetSign, int cost, object edgeValue)
		{
			IVertex source = this.FindVertex(sourceSign);
			IVertex target = this.FindVertex(targetSign);
			
			return this.AddDirectedEdge(source, target, cost, edgeValue);
		}

		public IEdge AddDirectedEdge(IVertex source, IVertex target, int cost, object edgeValue)
		{
			if (source == null || target == null || source == target) return null;

			IEdge resultEdge = this.fProvider.CreateEdge(source, target, cost, edgeValue);
			source.EdgesOut.Add(resultEdge);
			this.fEdgesList.Add(resultEdge);
			
			return resultEdge;
		}

		public void DeleteVertex(IVertex vertex)
		{
			if (vertex == null) return;

			for (int i = this.fEdgesList.Count - 1; i >= 0; i--)
			{
				IEdge edge = this.fEdgesList[i];

				if (edge.Source == vertex || edge.Target == vertex)
				{
					this.DeleteEdge(edge);
				}				
			}

			this.fVerticesList.Remove(vertex);
		}

		public void DeleteEdge(IEdge edge)
		{
			if (edge == null) return;

			IVertex src = edge.Source;
			src.EdgesOut.Remove(edge);

			this.fEdgesList.Remove(edge);
		}

		public IVertex FindVertex(string sign)
		{
			IVertex result;
			this.fVerticesDictionary.TryGetValue(sign, out result);
			return result;
		}

		#endregion

		#region Pathes search

		public void FindPathTree(IVertex root)
		{
			if (root == null) return;

			// reset path tree
			foreach (IVertex node in this.fVerticesList)
			{
				node.Dist = int.MaxValue;
				node.Visited = false;
				node.EdgeIn = null;
			}

			// init root
			root.Dist = 0;
			root.Visited = true;
			root.EdgeIn = null;

			PathCandidate topCandidate = new PathCandidate(root, null);

			// processing
			while (topCandidate != null)
			{
				IVertex topNode = topCandidate.Node;
				topCandidate = topCandidate.Next;

				int nodeDist = topNode.Dist;
				topNode.Visited = false;

				foreach (IEdge link in topNode.EdgesOut)
				{
                    IVertex target = link.Target;
                    int newDist = nodeDist + link.Cost;

					if (newDist < target.Dist)
					{
						target.Dist = newDist;
						target.EdgeIn = link;

						if (!target.Visited)
						{
							target.Visited = true;
							topCandidate = new PathCandidate(target, topCandidate);
						}
					}
				}
			}
		}

		public IEnumerable<IEdge> GetPath(IVertex target)
		{
			List<IEdge> result = new List<IEdge>();
			if (target == null) return result;

			IEdge edge = target.EdgeIn;
			while (edge != null)
			{
				result.Insert(0, edge);
				edge = edge.Source.EdgeIn;
			}

			return result;
		}

		#endregion
	}
}
