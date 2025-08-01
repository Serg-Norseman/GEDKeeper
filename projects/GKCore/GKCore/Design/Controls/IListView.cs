/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using GKCore.Lists;

namespace GKCore.Design.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public interface IListItem
    {
        //bool Checked { get; set; }
        object Tag { get; set; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IListView : IBaseControl
    {
        IListSource ListMan { get; set; }
        int SelectedIndex { get; set; }
        int SortColumn { get; set; }
        GKSortOrder SortOrder { get; set; }

        void AddColumn(string caption, int width, bool autoSize = false, GKHorizontalAlignment textAlign = GKHorizontalAlignment.Left);
        void AddCheckedColumn(string caption, int width, bool autoSize = false);
        void Clear();
        void ClearColumns();
        void DeleteRecord(object data);
        object GetSelectedData();
        IList<object> GetSelectedItems();
        void ResizeColumn(int columnIndex);
        void ResizeColumns();
        void SelectItem(int index);
        void SelectItem(object rowData);
        void SetColumnCaption(int index, string caption); // GK specific
        void SetSortColumn(int sortColumn, bool checkOrder = true);
        void SortModelColumn(int columnId);
        void UpdateContents(bool columnsChanged = false); // GK specific
    }
}
