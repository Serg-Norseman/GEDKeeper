/* CIMultimedia.cs
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

using System.Collections;

namespace GEDmill.Model
{
    /// <summary>
    /// Data structure encapsulating a multimedia file record, for sorting the attached multimedia into a user-specified order.
    /// </summary>
    public class Multimedia
    {
        public string Format;
        public string Title;
        public string FileName;
        public int Width;
        public int Height;
        public string LargeFileName;

        // Order value from CMultimediaFileReference, indicating user's preferred order. (0=top)
        public int Ordering;


        public Multimedia(int ordering, string format, string title, string filename, string largeFilename, int width, int height)
        {
            Format = format;
            Title = title;
            FileName = filename;
            Width = width;
            Height = height;
            LargeFileName = largeFilename;
            Ordering = ordering;
        }

        // Compares two multimedia objects based on user-specified order
        public class OrderComparer : IComparer
        {
            public int Compare(object x, object y)
            {
                Multimedia im1 = x as Multimedia;
                Multimedia im2 = y as Multimedia;
                int order1 = 0;
                int order2 = 0;

                if (im1 != null) {
                    order1 = im1.Ordering;
                }
                if (im2 != null) {
                    order2 = im2.Ordering;
                }

                if (im1 == null) {
                    if (im2 == null) {
                        return 0;
                    }
                    return 1;
                }

                if (im2 == null) {
                    return -1;
                }

                return order1 - order2;
            }
        }
    }
}
