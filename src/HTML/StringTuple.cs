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

namespace GEDmill.HTML
{
    /// <summary>
    /// Generally useful class for containing strings.
    /// Currently (10Dec08) used when creating the index, to store title, filename and extra text.
    /// </summary>
    public class StringTuple : IComparable<StringTuple>
    {
        public string First;
        public string Second;
        public string Third;


        public StringTuple(string first, string second)
        {
            First = first;
            Second = second;
            Third = "";
        }

        public StringTuple(string first, string second, string third)
        {
            First = first;
            Second = second;
            Third = third;
        }

        // First string is the main way to represent this object
        public override string ToString()
        {
            return First;
        }

        // Compares first string of two StringTuple instances
        public int CompareTo(StringTuple other)
        {
            if (other != null) {
                return First.CompareTo(other.First);
            }
            return 1;
        }
    }
}
