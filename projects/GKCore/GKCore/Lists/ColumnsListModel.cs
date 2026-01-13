/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Lists
{
    public sealed class ColumnsListModel : SimpleListModel<ListColumn>
    {
        public ColumnsListModel() :
            base(null, CreateListColumns())
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.ltNone);
            result.AddColumn(LSID.Enabled, DataType.dtBool, 40, true);
            result.AddColumn(LSID.NumberSym, DataType.dtString, 50, true);
            result.AddColumn(LSID.Title, DataType.dtString, 175, true);
            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fFetchedRec.CurActive;
                    break;
                case 1:
                    result = fFetchedRec.Order;
                    break;
                case 2:
                    result = fFetchedRec.ColName;
                    break;
            }
            return result;
        }

        protected override void SetColumnValueEx(ListColumn item, int colIndex, object value)
        {
            if (item != null && colIndex == 0 && value is bool chk)
                item.CurActive = chk;
        }
    }
}
