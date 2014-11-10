/* CYear.cs
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

namespace GEDmill
{
    // Encapsulates the various different representations of a year for presentation in the website.
    public class CYear
    {
        public int m_nYear;
        public int m_nYearAlt;
        public bool m_bAltValid;

        // Constructor
        public CYear( int year, int altyear )
        {
            
            m_nYear = year;
            m_nYearAlt = altyear;
            m_bAltValid = true;
        }

        // Constructor
        public CYear( int year )
        {
            m_nYear = year;
            m_nYearAlt = 0;
            m_bAltValid = false;
        }

        // Constructor
        public CYear( CYear year )
        {
            m_nYear = year.m_nYear;
            m_nYearAlt = year.m_nYearAlt;
            m_bAltValid = year.m_bAltValid;
        }

        // The main purpose of this class is to provide the best textual representation of a year. 
        // This method does it.
        public override string ToString()
        {
            int year = m_nYear;
            int altyear = m_nYearAlt;
            bool bc = false;
            if( year < 0 )
            {
                bc = true;
                year = -year;
            }
            if( altyear < 0 )
            {
                bc = true;
                altyear = -altyear;
            }

            string sYear;
            if( m_bAltValid )
            {
                sYear = CPGDate.Strokeise( year, altyear, "/" );
            }
            else
            {
                sYear = year.ToString();
            }
            if( bc )
            {
                sYear += " B.C.";
            }

            return sYear;
        }
    }
}
