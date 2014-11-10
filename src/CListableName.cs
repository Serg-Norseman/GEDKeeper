/* CListableName.cs
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
using GEDmill.LLClasses;

namespace GEDmill
{
    // Holds a name (both surname and first names) for use in a list box and provides sorting by surname then firstname.
    public class CListableName : ListViewItem.ListViewSubItem
    {
        // The string to display if no other name is present in this instance
        public static string s_sUnknownName;

        // The surname (for sorting)
        private string m_sSurname;

        // The first name(s)
        private string m_sFirstName;

        // The record that this item represents
        private CIndividualRecord m_ir;

        // Constructor
        public CListableName( CIndividualRecord ir, string surname, string firstname )
        {
            m_ir = ir;
            m_sSurname = surname;
            m_sFirstName = firstname;

            base.Text = ToString();
        }

        // Accessor
        public CIndividualRecord Individual
        {
            get
            {
                return m_ir;
            }
        }

        // To display the name in the list
        public override string ToString()
        {
            string name = "";
            if (m_sFirstName != "" && m_sSurname != "")
            {
                name = String.Concat(m_sSurname, ", ", m_sFirstName);
            }
            else if (m_sSurname != "")
            {
                name = m_sSurname;
            }
            else
            {
                name = m_sFirstName;
            }

            if (name == "")
            {
                name = s_sUnknownName;
            }

            return name;
        }

        // To sort the list
        public int CompareTo( CListableName other ) 
        {
            if( m_sSurname != "" && other.m_sSurname != "" )
            {
                int result = m_sSurname.CompareTo( other.m_sSurname );
                if (result != 0)
                {
                    return result;
                }
            }
            else if (m_sSurname != "")
            {
                return 1;
            }
            else if (other.m_sSurname != "")
            {
                return -1;
            }

            if (m_sFirstName != "" && other.m_sFirstName != "")
            {
                return m_sFirstName.CompareTo(other.m_sFirstName);
            }

            if (m_sFirstName != "")
            {
                return 1;
            }

            if (other.m_sFirstName != "")
            {
                return -1;
            }

            return 0;
        }
    }
}
