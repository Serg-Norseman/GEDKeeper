/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
    public enum LocationColumnType
    {
        ctName,
        ctLati,
        ctLong,
        ctChangeDate
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class LocationListMan : ListManager
    {
        private GDMLocationRecord fRec;


        public LocationListMan(IBaseContext baseContext) :
            base(baseContext, CreateLocationListColumns(), GDMRecordType.rtLocation)
        {
        }

        public static ListColumns CreateLocationListColumns()
        {
            var result = new ListColumns();

            NumberFormatInfo nfi = new NumberFormatInfo();
            nfi.NumberDecimalSeparator = ".";

            result.AddColumn(LSID.LSID_Title, DataType.dtString, 300, true, true);
            result.AddColumn(LSID.LSID_Latitude, DataType.dtFloat, 120, true, false, "0.000000", nfi);
            result.AddColumn(LSID.LSID_Longitude, DataType.dtFloat, 120, true, false, "0.000000", nfi);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = IsMatchesMask(fRec.LocationName, QuickFilter);

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (aRec as GDMLocationRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((LocationColumnType)colType)
            {
                case LocationColumnType.ctName:
                    result = fRec.LocationName;
                    break;

                case LocationColumnType.ctLati:
                    result = fRec.Map.Lati;
                    break;

                case LocationColumnType.ctLong:
                    result = fRec.Map.Long;
                    break;

                case LocationColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
