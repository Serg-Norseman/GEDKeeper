/* CListableBool.cs
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
    // Special class of ListViewItem can represent individual/source records.
    public class CListableBool : ListViewItem
    {
        // The record associated with this list item
        protected CISRecord m_isr;

        // What name to use if individual has no name
        public static string s_sUnknownName;

        // Surname of individual
        protected string m_sSurname;

        // Firstname of individual
        protected string m_sFirstName;

        // True if this list item has a check box
        protected bool m_bThisIsACheckBox;

        // Constructor from individual record
        public CListableBool( CIndividualRecord ir, string surname, string firstname, bool bThisIsACheckBox )
        {
            m_isr = ir;
            m_sSurname = surname;
            m_sFirstName = firstname;
            m_bThisIsACheckBox = bThisIsACheckBox;

            base.Text = ToString();
        }

        // Constructor
        public CListableBool( CISRecord isr, bool bThisIsACheckBox )
        {
            m_isr = isr;
            m_sSurname = "";
            m_sFirstName = "";
            m_bThisIsACheckBox = bThisIsACheckBox;

            base.Text = ToString();
        }

        // For displaying the list item
        public override string ToString()
        {
            string name = "";
            if( !m_bThisIsACheckBox )
            {
                if( m_isr is CSourceRecord )
                {
                    return m_isr.ToString();
                }
                else
                {
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
                }
            }

            return name;
        }

        // For sorting the list
        public int CompareTo( CListableBool other ) 
        {
            // Assumption here is that other.m_bThisIsACheckBox will be the same (i.e. list is homogeneous)
            if( !m_bThisIsACheckBox ) 
            {
                // Assumption here is that other is also CSourceRecord (i.e. list is homogeneous)
                if( m_isr is CSourceRecord ) 
                {
                    return ((CSourceRecord)m_isr).DescriptiveTitle.CompareTo( ((CSourceRecord)other.m_isr).DescriptiveTitle );
                }
                else
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
            else
            {
                if( m_isr == null && other.m_isr == null )
                {
                    return 0;
                }
                if( m_isr == null )
                {
                    return 1;
                }
                if( other.m_isr == null )
                {
                    return -1;
                }
                bool tr = m_isr.Restricted;
                bool or = other.m_isr.Restricted;
                if( tr == or )
                {
                    return 0;
                }
                if( tr )
                {
                    return 1;
                }
                return -1;
            }
        }

        // Used to exclude the record from the generated web site
        public void SetRestricted( bool bRestricted )
        {
            m_isr.Restricted = bRestricted;
        }

        // Accessor
        public CISRecord ISRecord
        {
            get
            {
                return m_isr;
            }
        }
    }

}
