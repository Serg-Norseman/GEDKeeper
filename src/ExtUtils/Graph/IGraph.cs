using System;
using System.Collections.Generic;

namespace ExtUtils.Graph
{
	public interface IGraph
	{
		IEnumerable<IVertex> Vertices { get; }
		IEnumerable<IEdge> Edges { get; }
	}
}
