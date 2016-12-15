/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon.GEDCOM;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// This class is wrapper for the GEDCOM dates to a ListView items.
    /// </summary>
    public class GEDCOMDateItem : IComparable
    {
        private readonly GEDCOMCustomDate fDate;

        public GEDCOMDateItem(GEDCOMCustomDate date)
        {
            this.fDate = date;
        }

        public override string ToString()
        {
            string strVal;

            if (this.fDate == null) {
                strVal = "";
            } else {
                GlobalOptions glob = GlobalOptions.Instance;
                strVal = GKUtils.GetCustomDateFmtString(this.fDate, glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
            }

            return strVal;
        }

        public int CompareTo(object obj)
        {
            GEDCOMDateItem otherItem = obj as GEDCOMDateItem;
            if (otherItem == null) {
                return -1;
            }

            IComparable cv1 = this.fDate;
            IComparable cv2 = otherItem.fDate;

            int compRes;
            if (cv1 != null && cv2 != null)
            {
                compRes = cv1.CompareTo(cv2);
            }
            else if (cv1 != null)
            {
                compRes = -1;
            }
            else if (cv2 != null)
            {
                compRes = 1;
            }
            else {
                compRes = 0;
            }
            return compRes;
        }
    }
}
