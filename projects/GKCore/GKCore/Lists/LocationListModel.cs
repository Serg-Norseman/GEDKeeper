/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Interfaces;
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


        public LocationListModel(IBaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtLocation)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtLocation);

            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Title, DataType.dtString, 300, true, true);
            result.AddColumn("Ext", DataType.dtString, 60, true, false);
            result.AddColumn(LSID.Latitude, DataType.dtFloat, 120, true, false, GEDCOMUtils.CoordFormat, GEDCOMUtils.CoordNumberFormatInfo);
            result.AddColumn(LSID.Longitude, DataType.dtFloat, 120, true, false, GEDCOMUtils.CoordFormat, GEDCOMUtils.CoordNumberFormatInfo);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        private bool CheckQuickFilter()
        {
            var quickFilter = base.QuickFilter;

            var names = fFetchedRec.Names;
            for (int i = 0; i < names.Count; i++) {
                var locName = names[i].StringValue;

                bool res;
                if (quickFilter.Type == MatchType.Indistinct) {
                    res = (IndistinctMatching.GetSimilarity(locName, quickFilter.Value) >= quickFilter.IndistinctThreshold);
                } else {
                    res = IsMatchesMask(locName, quickFilter.Value);
                }

                if (res) {
                    return true;
                }
            }

            return false;
        }

        public override bool CheckFilter()
        {
            bool res = CheckQuickFilter();

            res = res && CheckCommonFilter() && CheckExternalFilter(fFetchedRec);

            return res;
        }

        protected override bool CheckCommonCondition(FilterCondition fcond)
        {
            if ((ColumnType)fcond.ColumnIndex == ColumnType.ctName) {
                var names = fFetchedRec.Names;
                for (int i = 0; i < names.Count; i++) {
                    if (CheckCondition(fcond, names[i].StringValue)) {
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
