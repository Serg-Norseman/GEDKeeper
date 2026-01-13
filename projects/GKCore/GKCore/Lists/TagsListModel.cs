/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Options;

namespace GKCore.Lists
{
    public sealed class TagsListModel : SimpleListModel<GDMTag>
    {
        public TagsListModel(BaseContext baseContext, string title) :
            base(baseContext, CreateListColumns(title))
        {
        }

        public static ListColumns CreateListColumns(string title)
        {
            var result = new ListColumns(GKListType.ltNone);
            result.AddColumn(title, DataType.dtString, 350, false);
            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fFetchedRec.StringValue;
                    break;
            }
            return result;
        }
    }
}
