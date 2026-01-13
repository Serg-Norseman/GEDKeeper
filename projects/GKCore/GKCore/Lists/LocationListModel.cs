/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Filters;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class LocationListModel : RecordsListModel<GDMLocationRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctName,
            ctExt,
            ctLati,
            ctLong,
            ctChangeDate
        }


        public LocationListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtLocation)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtLocation);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Title, DataType.dtString, 300, true);
            result.AddColumn("Ext", DataType.dtString, 60, false);
            result.AddColumn(LangMan.LS(LSID.Latitude), DataType.dtFloat, 120, false, GEDCOMUtils.CoordFormat, GEDCOMUtils.CoordNumberFormatInfo);
            result.AddColumn(LangMan.LS(LSID.Longitude), DataType.dtFloat, 120, false, GEDCOMUtils.CoordFormat, GEDCOMUtils.CoordNumberFormatInfo);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            return result;
        }

        protected override bool CheckQuickFilter()
        {
            var names = fFetchedRec.Names;
            for (int i = 0, num = names.Count; i < num; i++) {
                if (CheckQuickFilter(names[i].StringValue)) {
                    return true;
                }
            }
            return false;
        }

        protected override bool CheckCommonCondition(ColumnConditionExpression fcond)
        {
            if ((ColumnType)fcond.ColumnIndex == ColumnType.ctName) {
                var names = fFetchedRec.Names;
                for (int i = 0; i < names.Count; i++) {
                    if (ListFilter.CheckCondition(fcond, names[i].StringValue)) {
                        return true;
                    }
                }
                return false;
            } else {
                return base.CheckCommonCondition(fcond);
            }
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctName:
                    result = fFetchedRec.GetNameByDate(null, true);
                    break;

                case ColumnType.ctExt:
                    result = (fFetchedRec.Names.Count > 1 || fFetchedRec.TopLevels.Count > 0) ? GKData.CHECK_MARK : string.Empty;
                    break;

                case ColumnType.ctLati:
                    result = fFetchedRec.Map.Lati;
                    break;

                case ColumnType.ctLong:
                    result = fFetchedRec.Map.Long;
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
