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

using System.Windows.Forms;
using GKCommon.GEDCOM;

namespace GEDmill.ListView
{
    /// <summary>
    /// Holds a year for use in a list box and provides sorting by year.
    /// </summary>
    public class CListableYear : ListViewItem.ListViewSubItem
    {
        // Date holds the year
        protected GEDCOMDateValue fDate;

        public CListableYear(GEDCOMDateValue date)
        {
            fDate = date;
            base.Text = ToString();
        }

        // Used for guesses made from parent's death year
        public CListableYear(GEDCOMDateValue date, bool bBefore) : this(date)
        {
        }

        // String to display in list control
        public override string ToString()
        {
            var xdate = (fDate == null) ? null : fDate.Value as GEDCOMDate;
            if (xdate == null || xdate.Year == 0)
                return "";

            return xdate.Year.ToString();
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

        // Accessor
        public GEDCOMDateValue Date
        {
            get {
                return fDate;
            }
        }
    }
}
