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
using System.Collections.Generic;

namespace GKCommon.SmartGraph
{
    public class Vertex : IVertex
    {
        public string Sign { get; set; }
        public object Value { get; set; }

        // path-search runtime
        public int Dist { get; set; }
        public bool Visited { get; set; }

        public IEdge EdgeIn { get; set; }
        public List<IEdge> EdgesOut { get; private set; }

        public Vertex()
        {
            EdgesOut = new List<IEdge>();
        }

        public int CompareTo(object obj)
        {
            if (!(obj is Vertex))
                throw new ArgumentException("Cannot compare two objects");

            return GetHashCode().CompareTo(obj.GetHashCode());
        }
    }
}
