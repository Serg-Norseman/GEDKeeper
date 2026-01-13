/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
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

        public override int GetHashCode()
        {
            unchecked {
                int hashCode = -846288654;
                hashCode = hashCode * -1521134295 + EqualityComparer<GDMIndividualRecord>.Default.GetHashCode(IndiRec);
                hashCode = hashCode * -1521134295 + ChartKind.GetHashCode();
                return hashCode;
            }
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
