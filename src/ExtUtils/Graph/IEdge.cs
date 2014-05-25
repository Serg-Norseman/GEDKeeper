using System;

namespace ExtUtils.Graph
{
	public interface IEdge : IComparable
	{
		int Cost { get; }
		IVertex Source { get; }
		IVertex Target { get; }
		object Value { get; }
	}
}
