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
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Options;

namespace GKUI.Controls
{
    /// <summary>
    /// 
    /// </summary>
    [Serializable]
    public class GKListItem : ListViewItem, IComparable
    {
        protected object fValue;

        public object Data;

        public GKListItem(object itemValue, object data)
        {
            this.fValue = itemValue;
            base.Text = this.ToString();
            this.Data = data;
        }

        public override string ToString()
        {
            string strVal = this.fValue.ToString();
            return strVal;
        }

        public int CompareTo(object obj)
        {
            GKListItem otherItem = obj as GKListItem;
            if (otherItem == null) {
                return -1;
            }

            IComparable cv1 = this.fValue as IComparable;
            IComparable cv2 = otherItem.fValue as IComparable;

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

        public void AddSubItem(object itemValue)
        {
            GKListSubItem subItem = new GKListSubItem(itemValue);
            this.SubItems.Add(subItem);
        }
    }


    public class GKListSubItem : ListViewItem.ListViewSubItem, IComparable
    {
        protected object fValue;

        public GKListSubItem(object itemValue)
        {
            this.fValue = itemValue;
            base.Text = this.ToString();
        }

        public override string ToString()
        {
            string strVal;

            if (this.fValue == null) {
                strVal = "";
            } else {
                GEDCOMCustomDate customDate = this.fValue as GEDCOMCustomDate;
                if (customDate != null) {
                    GlobalOptions glob = GlobalOptions.Instance;
                    strVal = GKUtils.GetCustomDateFmtString(customDate, glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
                } else {
                    strVal = this.fValue.ToString();
                }
            }

            return strVal;
        }

        public int CompareTo(object obj)
        {
            GKListSubItem otherItem = obj as GKListSubItem;
            if (otherItem == null) {
                return -1;
            }

            IComparable cv1 = this.fValue as IComparable;
            IComparable cv2 = otherItem.fValue as IComparable;

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
