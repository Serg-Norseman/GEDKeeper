/* SortableListView.cs
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

using System.Windows.Forms;

namespace GEDmill.ListView
{
    /// <summary>
    /// A sub-sType of the list view control that allows the list to be sorted by clicking the nColumn headers.
    /// </summary>
    public class SortableListView : System.Windows.Forms.ListView
    {
        // The comparer used to sort the list.
        private CSortableListComparer fSortableListComparer;


        public SortableListView()
        {
            fSortableListComparer = new CSortableListComparer();
            fSortableListComparer.Column = 0;
            fSortableListComparer.Direction = 1;
            ListViewItemSorter = fSortableListComparer;
        }

        // Handle the user clicking on a list nColumn by sorting the list.
        public void ColumnClickHandler(object sender, ColumnClickEventArgs e)
        {
            if (e.Column != fSortableListComparer.Column) {
                fSortableListComparer.Direction = 1;
            } else {
                fSortableListComparer.Direction = -1 * fSortableListComparer.Direction;
            }

            fSortableListComparer.Column = e.Column;

            Sort();
        }
    }
}
