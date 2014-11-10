/* CStringTuple.cs
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

namespace GEDmill.HTMLClasses
{
    // Generally useful class for containing strings.
    // Currently (10Dec08) used when creating the index, to store title, filename and extra text.
    public class CStringTuple : IComparable
    {
        public string m_sFirst;
        public string m_sSecond;
        public string m_sThird;

        // Constructor
        public CStringTuple( string first, string second )
        {
            
            m_sFirst = first;
            m_sSecond = second;
            m_sThird = "";
        }

        // Constructor
        public CStringTuple( string first, string second, string third )
        {
            
            m_sFirst = first;
            m_sSecond = second;
            m_sThird = third;
        }
        
        // First string is the main way to represent this object
        public override string ToString()
        {
            return m_sFirst;
        }

        // Compares first string of two CStringTuple instances
        public int CompareTo(object obj)
        {
            if( obj != null )
            {
                if( obj is CStringTuple )
                {
                    int res = m_sFirst.CompareTo( ((CStringTuple)obj).m_sFirst );
                    return res;
                }
                throw new ArgumentException("Object is not a CStringTuple(250)");    
            }
            return 1;
        }
    }
}
