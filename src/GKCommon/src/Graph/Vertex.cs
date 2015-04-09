using System;
using System.Collections.Generic;

namespace GKCommon.Graph
{
	public class Vertex : IVertex
	{
    	//private static int nextNodeIdx = 1;
        //public int Idx;

        public string Sign { get; set; }
		public List<IEdge> EdgesOut { get; private set; }
		public object Value { get; set; }

		// path-search runtime
		public int Dist { get; set; }
		public bool Visited { get; set; }
		public IEdge EdgeIn { get; set; }

		public Vertex()
		{
			//this.Idx = nextNodeIdx++;

			this.EdgesOut = new List<IEdge>();
		}

		public int CompareTo(object obj)
		{
			if (!(obj is Vertex))
				throw new ArgumentException("Cannot compare two objects");

			return GetHashCode().CompareTo(obj.GetHashCode());
		}
	}
}
