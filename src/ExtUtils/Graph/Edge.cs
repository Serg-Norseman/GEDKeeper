using System;

namespace ExtUtils.Graph
{
	public class Edge : IEdge
	{
		#region Private fields

		//private int fCost;
		private Vertex fSource;
		private Vertex fTarget;
		//private object fValue;

		#endregion

		#region Properties

		public int Cost { get; private set; }
		public object Value { get; private set; }

		public Vertex Source
		{
			get {
				return fSource;
			}
		}

		IVertex IEdge.Source
		{
			get {
				return (IVertex)this.Source;
			}
		}

		public Vertex Target
		{
			get {
				return fTarget;
			}
		}

		IVertex IEdge.Target
		{
			get {
				return (IVertex)this.Target;
			}
		}

		#endregion

		public Edge(Vertex source, Vertex target, int cost, object value)
		{
			if (source == null)
				throw new ArgumentNullException("source");
			if (target == null)
				throw new ArgumentNullException("target");

			this.fSource = source;
			this.fTarget = target;

			this.Cost = cost;
			this.Value = value;
		}

		public int CompareTo(object obj)
		{
			if (!(obj is Edge))
				throw new ArgumentException("Cannot compare two objects");

			return GetHashCode().CompareTo(obj.GetHashCode());
		}
	}
}
