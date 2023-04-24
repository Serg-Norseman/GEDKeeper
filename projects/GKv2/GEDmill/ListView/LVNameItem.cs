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
    /// Holds a name (both surname and first names) for use in a list box and provides sorting by surname then firstname.
    /// </summary>
    public class LVNameItem : ListViewItem.ListViewSubItem, IComparable, IComparable<LVNameItem>
    {
        // The surname (for sorting)
        private string fSurname;

        // The first name(s)
        private string fFirstName;


        public LVNameItem(string surname, string firstname)
        {
            fSurname = surname;
            fFirstName = firstname;

            base.Text = ToString();
        }

        // To display the name in the list
        public override string ToString()
        {
            string name;
            if (fFirstName != "" && fSurname != "") {
                name = string.Concat(fSurname, ", ", fFirstName);
            } else if (fSurname != "") {
                name = fSurname;
            } else {
                name = fFirstName;
            }

            if (name == "") {
                name = GMConfig.Instance.UnknownName;
            }

            return name;
        }

        public int CompareTo(object obj)
        {
            return CompareTo((LVNameItem)obj);
        }

        // To sort the list
        public int CompareTo(LVNameItem other)
        {
            if (fSurname != "" && other.fSurname != "") {
                int result = fSurname.CompareTo(other.fSurname);
                if (result != 0) {
                    return result;
                }
            } else if (fSurname != "") {
                return 1;
            } else if (other.fSurname != "") {
                return -1;
            }

            if (fFirstName != "" && other.fFirstName != "") {
                return fFirstName.CompareTo(other.fFirstName);
            }

            if (fFirstName != "") {
                return 1;
            }

            if (other.fFirstName != "") {
                return -1;
            }

            return 0;
        }
    }
}
