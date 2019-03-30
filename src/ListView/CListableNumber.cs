/* CListableNumber.cs
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

namespace GEDmill.ListView
{
    /// <summary>
    /// Special type of ListViewSubItem that stores a number in both a numeric and a text form. 
    /// The numeric form is used for sorting. (Used to display number of items and total, e.g. 10/50, 11/50 etc)
    /// </summary>
    public class CListableNumber : ListViewItem.ListViewSubItem
    {
        // Numeric representation
        protected int fNumber;

        // Text representation
        protected string fString;


        public CListableNumber(int number, string s)
        {
            fNumber = number;
            fString = s;
            base.Text = s;
        }

        // For displaying the list item
        public override string ToString()
        {
            return fString;
        }

        // Returns -ve if this instance is less than other...
        public int CompareTo(CListableNumber other)
        {
            return fNumber - other.fNumber;
        }
    }
}
