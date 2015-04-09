using System.Collections.Generic;

namespace GKCommon.Graph
{
	public interface IGraph
	{
		IEnumerable<IVertex> Vertices { get; }
		IEnumerable<IEdge> Edges { get; }
	}
}
