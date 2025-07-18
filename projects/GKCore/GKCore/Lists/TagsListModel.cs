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

using GDModel;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore.Lists
{
    public sealed class TagsListModel : SimpleListModel<GDMTag>
    {
        public TagsListModel(IBaseContext baseContext, string title) :
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
