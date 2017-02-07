/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;

namespace GKCommon
{
    public struct Range<T> where T : IComparable<T>
    {
        public readonly T Start;
        public readonly T End;

        public Range(T start, T end)
        {
            if (start.CompareTo(end) > 0)
                throw new ArgumentException("End must be greater than Start");

            this.Start = start;
            this.End = end;
        }

        public bool IsOverlapped(Range<T> other)
        {
            if (this.Start.CompareTo(other.Start) == 0)
            {
                return true;
            }
            
            if (this.Start.CompareTo(other.Start) > 0)
            {
                return this.Start.CompareTo(other.End) <= 0;
            }
            
            return other.Start.CompareTo(this.End) <= 0;
        }
    }
}
