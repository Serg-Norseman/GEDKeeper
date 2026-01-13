/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
