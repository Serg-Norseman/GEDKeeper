/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FamilyListModel : RecordsListModel<GDMFamilyRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctFamilyStr,
            ctMarriageDate,
            ctChangeDate
        }


        public FamilyListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtFamily)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtFamily);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Spouses, DataType.dtString, 300, true);
            result.AddColumn(LSID.MarriageDate, DataType.dtString, 100, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            return result;
        }

        protected override string GetQuickFilterBuffer()
        {
            return GKUtils.GetFamilyString(fBaseContext.Tree, fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctFamilyStr:
                    result = GKUtils.GetFamilyString(fBaseContext.Tree, fFetchedRec);
                    break;

                case ColumnType.ctMarriageDate:
                    result = GetDateValue(GKUtils.GetMarriageDate(fFetchedRec), isVisible);
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
