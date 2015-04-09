using System;

namespace GKCommon.Graph
{
	public interface IEdge : IComparable
	{
		int Cost { get; }
		IVertex Source { get; }
		IVertex Target { get; }
		object Value { get; }
	}
}
