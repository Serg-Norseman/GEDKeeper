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
using GKCore.Media;

namespace GEDmill.Model
{
    /// <summary>
    /// Data structure encapsulating a multimedia file record, for sorting the attached multimedia into a user-specified order.
    /// </summary>
    public class Multimedia : IComparable, IComparable<Multimedia>
    {
        public MultimediaKind Format;
        public string Title;
        public string FileName;
        public int Width;
        public int Height;
        public string LargeFileName;

        // Order value from CMultimediaFileReference, indicating user's preferred order. (0=top)
        public int Ordering;


        public Multimedia(int ordering, MultimediaKind format, string title, string filename, string largeFilename, int width, int height)
        {
            Format = format;
            Title = title;
            FileName = filename;
            Width = width;
            Height = height;
            LargeFileName = largeFilename;
            Ordering = ordering;
        }

        public int CompareTo(object obj)
        {
            return CompareTo((Multimedia)obj);
        }

        // For sorting the list
        public int CompareTo(Multimedia other)
        {
            if (other == null) {
                return -1;
            }

            return this.Ordering - other.Ordering;
        }
    }
}
