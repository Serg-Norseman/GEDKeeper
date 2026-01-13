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
    public sealed class RecsListModel : SimpleListModel<GDMRecord>
    {
        private readonly bool fShowXRefs;

        public RecsListModel(BaseContext baseContext, string title, bool showXRefs = true) :
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
