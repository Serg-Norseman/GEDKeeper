using System;
using System.Collections.Generic;

namespace ExtUtils.Graph
{
	public interface IVertex : IComparable
	{
		string Sign { get; set; }
		List<IEdge> EdgesOut { get; }
		object Value { get; set; }

		int Dist { get; set; }
		bool Visited { get; set; }
		IEdge EdgeIn { get; set; }
	}
}
