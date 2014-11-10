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

namespace GEDmill
{
    // Holds a year for use in a list box and provides sorting by year.
    public class CListableYear : ListViewItem.ListViewSubItem
    {
        // Date holds the year
        protected CPGDate m_date;

        // Constructor
        public CListableYear( CPGDate date )
        {
            m_date = date;
            base.Text = ToString();
        }

        // Used for guesses made from parent's death year
        public CListableYear( CPGDate date, bool bBefore ) 
        {
            if( date != null )
            {
                m_date = new CPGDate( date );
                if( bBefore )
                {
                    m_date.m_etType = CPGDate.EType.RangeBefore;
                    base.Text = String.Concat("prob bef ", date.Year );
                }
                else
                {
                    m_date.m_etType = CPGDate.EType.RangeAfter;
                    base.Text = String.Concat("prob aft ", date.Year );
                }
            }
            else
            {
                m_date = null;
            }

        }

        // String to display in list control
        public override string ToString()
        {
            if( m_date == null || m_date.m_year == null )
                return "";

            return m_date.YearString();
        }

        // Comparer for sorting list
        public int CompareTo( CListableYear other ) 
        {
            if( m_date == null )
                return -1;
            if( other.m_date == null )
                return 1;

            return m_date.CompareTo( other.m_date );
        }

        // Accessor
        public CPGDate Date
        {
            get
            {
                return m_date;
            }
        }
    }
}
