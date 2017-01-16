/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public enum LocationColumnType
    {
        lctName,
        lctLati,
        lctLong,
        lctChangeDate
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class LocationListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            NumberFormatInfo nfi = new NumberFormatInfo();
            nfi.NumberDecimalSeparator = ".";

            this.AddStatic(LSID.LSID_Title, DataType.dtString, 300, true);
            this.AddStatic(LSID.LSID_Latitude, DataType.dtFloat, 120, true, "0.000000", nfi);
            this.AddStatic(LSID.LSID_Longitude, DataType.dtFloat, 120, true, "0.000000", nfi);
            this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
        }

        public LocationListColumns()
        {
            InitData(typeof(LocationColumnType));
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class LocationListMan : ListManager
    {
        private GEDCOMLocationRecord fRec;

        public override bool CheckFilter(ShieldState shieldState)
        {
            bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.LocationName, this.QuickFilter));

            res = res && base.CheckCommonFilter();

            return res;
        }

        public override void Fetch(GEDCOMRecord aRec)
        {
            this.fRec = (aRec as GEDCOMLocationRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = this.fRec.LocationName;
                    break;
                case 1:
                    result = this.fRec.Map.Lati;
                    break;
                case 2:
                    result = this.fRec.Map.Long;
                    break;
                case 3:
                    result = this.fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }

        public LocationListMan(GEDCOMTree tree) : base(tree, new LocationListColumns())
        {
        }
    }
}
