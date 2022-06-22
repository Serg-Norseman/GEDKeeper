/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.Globalization;
using GDModel;
using GKCore.Interfaces;

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
            ctLati,
            ctLong,
            ctChangeDate
        }


        public LocationListModel(IBaseContext baseContext) :
            base(baseContext, CreateLocationListColumns(), GDMRecordType.rtLocation)
        {
        }

        public static ListColumns<GDMLocationRecord> CreateLocationListColumns()
        {
            var result = new ListColumns<GDMLocationRecord>();

            NumberFormatInfo nfi = new NumberFormatInfo();
            nfi.NumberDecimalSeparator = ".";

            result.AddColumn(LSID.LSID_NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.LSID_Title, DataType.dtString, 300, true, true);
            result.AddColumn(LSID.LSID_Latitude, DataType.dtFloat, 120, true, false, "0.000000", nfi);
            result.AddColumn(LSID.LSID_Longitude, DataType.dtFloat, 120, true, false, "0.000000", nfi);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = IsMatchesMask(fFetchedRec.LocationName, QuickFilter);

            res = res && CheckCommonFilter() && CheckExternalFilter(fFetchedRec);

            return res;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctName:
                    result = fFetchedRec.LocationName;
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
