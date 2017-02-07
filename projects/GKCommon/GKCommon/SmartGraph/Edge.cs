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

namespace GKCommon.SmartGraph
{
	public class Edge : IEdge
	{
		#region Private fields

		private readonly int fCost;
		private readonly Vertex fSource;
		private readonly Vertex fTarget;
		private readonly object fValue;

		#endregion

		#region Properties

		public int Cost
		{
			get { return this.fCost; }
		}

		public object Value
		{
			get { return this.fValue; }
		}

		public Vertex Source
		{
			get { return this.fSource; }
		}

		IVertex IEdge.Source
		{
			get { return this.fSource; }
		}

		public Vertex Target
		{
			get { return this.fTarget; }
		}

		IVertex IEdge.Target
		{
			get { return this.fTarget; }
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
			this.fCost = cost;
			this.fValue = value;
		}

		public int CompareTo(object obj)
		{
			if (!(obj is Edge))
				throw new ArgumentException("Cannot compare two objects");

			return GetHashCode().CompareTo(obj.GetHashCode());
		}
	}
}
