/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GDModel;

namespace GKCore.Charts
{
    public sealed class TreeChartNavItem : IEquatable<TreeChartNavItem>
    {
        public GDMIndividualRecord IndiRec;
        public TreeChartKind ChartKind;

        public TreeChartNavItem(GDMIndividualRecord indiRec, TreeChartKind chartKind)
        {
            IndiRec = indiRec;
            ChartKind = chartKind;
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as TreeChartNavItem);
        }

        public bool Equals(TreeChartNavItem other)
        {
            return (other != null) && IndiRec == other.IndiRec && ChartKind == other.ChartKind;
        }

        public static bool operator ==(TreeChartNavItem item, TreeChartNavItem other)
        {
            return !ReferenceEquals(item, null) && !ReferenceEquals(other, null) && item.IndiRec == other.IndiRec && item.ChartKind == other.ChartKind;
        }

        public static bool operator !=(TreeChartNavItem item, TreeChartNavItem other)
        {
            return !(item == other);
        }
    }
}
