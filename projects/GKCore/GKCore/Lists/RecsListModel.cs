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
using GKCore.Options;

namespace GKCore.Lists
{
    public sealed class RecsListModel : SimpleListModel<GDMRecord>
    {
        private readonly bool fShowXRefs;

        public RecsListModel(IBaseContext baseContext, string title, bool showXRefs = true) :
            base(baseContext, CreateListColumns(title))
        {
            fShowXRefs = showXRefs;
        }

        public static ListColumns CreateListColumns(string title)
        {
            var result = new ListColumns(GKListType.ltNone);
            result.AddColumn(title, DataType.dtString, 300, false);
            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    if (fShowXRefs) {
                        result = $"[{fFetchedRec.XRef}] {GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec, false)}";
                    } else {
                        result = GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec, false);
                    }
                    break;
            }
            return result;
        }
    }
}
