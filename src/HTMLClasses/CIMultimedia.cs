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

using System;
using System.Collections;

namespace GEDmill.HTMLClasses
{
    // Data structure encapsulating a multimedia file record, for sorting the attached multimedia into a user-specified order.
    public class CIMultimedia
    {
        public string m_sFormat;
        public string m_sTitle;
        public string m_sFilename;
        public int m_nWidth;
        public int m_nHeight;
        public string m_sLargeFilename;

        // Order value from CMultimediaFileReference, indicating user's preferred order. (0=top)
        public int m_nOrdering; 

        // Constructor
        public CIMultimedia( int ordering, string format, string title, string filename, string largeFilename, int width, int height )
        {
            m_sFormat = format;
            m_sTitle = title;
            m_sFilename = filename;
            m_nWidth = width;
            m_nHeight = height;
            m_sLargeFilename = largeFilename;
            m_nOrdering = ordering;
        }

        // Compares two multimedia objects based on user-specified order
        public class OrderComparer : IComparer
        {
            public int Compare(object x, object y) 
            {
                CIMultimedia im1 = null;
                CIMultimedia im2 = null;
                int order1 = 0;
                int order2 = 0;

                if( x != null && x is CIMultimedia )
                {
                    im1 = (CIMultimedia)x;
                    order1 = im1.m_nOrdering;
                }
                if( y != null && y is CIMultimedia )
                {
                    im2 = (CIMultimedia)y;
                    order2 = im2.m_nOrdering;
                }

                if( im1 == null )
                {
                    if( im2 == null )
                    {
                        return 0;
                    }
                    return 1;
                }
                
                if( im2 == null )
                {
                    return -1;
                }

                return order1 - order2;
            }
        }   
    }
}
