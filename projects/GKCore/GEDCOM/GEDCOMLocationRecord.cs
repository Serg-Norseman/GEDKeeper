/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMLocationRecord : GEDCOMRecord
    {
        public GEDCOMMap Map
        {
            get { return TagClass(GEDCOMTagType.MAP, GEDCOMMap.Create) as GEDCOMMap; }
        }

        public string LocationName
        {
            get { return GetTagStringValue("NAME"); }
            set { SetTagStringValue("NAME", value); }
        }


        public GEDCOMLocationRecord(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetRecordType(GEDCOMRecordType.rtLocation);
            SetName(GEDCOMTagType._LOC);
        }

        // TODO: connect to use
        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMLocationRecord otherLoc = tag as GEDCOMLocationRecord;
            if (otherLoc == null) return 0.0f;

            float match = GetStrMatch(LocationName, otherLoc.LocationName, matchParams);
            return match;
        }
    }
}
