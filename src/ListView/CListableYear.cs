/* CListableYear.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;
using System.Windows.Forms;
using GDModel;
using GKCore.Types;

namespace GEDmill.ListView
{
    /// <summary>
    /// Holds a year for use in a list box and provides sorting by year.
    /// </summary>
    public class CListableYear : ListViewItem.ListViewSubItem, IComparable, IComparable<CListableYear>
    {
        // Date holds the year
        private GDMDateValue fDate;

        public CListableYear(GDMDateValue date)
        {
            fDate = date;
            base.Text = ToString();
        }

        // String to display in list control
        public override string ToString()
        {
            var xdate = (fDate == null) ? "" : fDate.GetDisplayStringExt(DateFormat.dfYYYY, false, false);
            return xdate;
        }

        public int CompareTo(object obj)
        {
            return CompareTo((CListableYear)obj);
        }

        // Comparer for sorting list
        public int CompareTo(CListableYear other)
        {
            if (fDate == null)
                return -1;
            if (other.fDate == null)
                return 1;

            return fDate.CompareTo(other.fDate);
        }
    }
}
