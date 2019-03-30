/* CSortableListComparer.cs
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
using System.Windows.Forms;

namespace GEDmill.ListView
{
    // A comparer used by the CSortableListView. Sorts the list according to the datatypes it contains.
    public class CSortableListComparer : IComparer
    {
        // Which nColumn is currently sorted
        protected int m_nCol;

        // The current sort order
        protected int m_nDirection;

        // Default constructor
        public CSortableListComparer()
        {
            m_nCol = 0;
        }

        // Constructor
        public CSortableListComparer(int nColumn, int nDirection)
        {
            m_nCol = nColumn;
            m_nDirection = nDirection;
        }

        // The main compare function
        public int Compare(object x, object y)
        {
            int nResult = 0;
            ListViewItem lx = (ListViewItem)x;
            ListViewItem ly = (ListViewItem)y;
            if (m_nCol == 0) {
                // Column 0 is always a listable bool
                nResult = ((CListableBool)(lx)).CompareTo((CListableBool)(ly));
            } else {
                // Must be a sub-item
                ListViewItem.ListViewSubItem lsx = lx.SubItems[m_nCol];
                ListViewItem.ListViewSubItem lsy = ly.SubItems[m_nCol];

                if (lsx is CListableName) {
                    nResult = ((CListableName)lsx).CompareTo((CListableName)lsy);
                } else if (lsx is CListableYear) {
                    nResult = ((CListableYear)lsx).CompareTo((CListableYear)lsy);
                } else if (lsx is CListableString) {
                    nResult = ((CListableString)lsx).CompareTo((CListableString)lsy);
                } else if (lsx is CListableNumber) {
                    nResult = ((CListableNumber)lsx).CompareTo((CListableNumber)lsy);
                } else {
                    nResult = lsx.Text.CompareTo(lsy.Text);
                }
            }

            return nResult * m_nDirection;
        }

        // Accessor
        public int Column
        {
            get {
                return m_nCol;
            }
            set {
                m_nCol = value;
            }
        }

        // Accessor
        public int Direction
        {
            get {
                return m_nDirection;
            }
            set {
                m_nDirection = value;
            }
        }
    }
}