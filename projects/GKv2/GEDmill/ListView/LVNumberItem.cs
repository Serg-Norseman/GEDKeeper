/* 
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
 */

using System;
using System.Windows.Forms;

namespace GEDmill.ListView
{
    /// <summary>
    /// Special type of ListViewSubItem that stores a number in both a numeric and a text form. 
    /// The numeric form is used for sorting. (Used to display number of items and total, e.g. 10/50, 11/50 etc)
    /// </summary>
    public class LVNumberItem : ListViewItem.ListViewSubItem, IComparable, IComparable<LVNumberItem>
    {
        // Numeric representation
        protected int fNumber;

        // Text representation
        protected string fString;


        public LVNumberItem(int number, bool zeroVisible = true)
        {
            fNumber = number;
            fString = (number == 0 && !zeroVisible) ? string.Empty : number.ToString();
            base.Text = fString;
        }

        public LVNumberItem(int number, string s)
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

        public int CompareTo(object obj)
        {
            return CompareTo((LVNumberItem)obj);
        }

        // Returns -ve if this instance is less than other...
        public int CompareTo(LVNumberItem other)
        {
            return fNumber - other.fNumber;
        }
    }
}
