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

using System;
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMLocationRecord : GDMRecord
    {
        private string fLocationName;
        private GDMMap fMap;


        public string LocationName
        {
            get { return fLocationName; }
            set { fLocationName = value; }
        }

        public GDMMap Map
        {
            get { return fMap; }
        }


        public GDMLocationRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtLocation);
            SetName(GEDCOMTagType._LOC);

            fMap = new GDMMap(this);
        }

        public override void Assign(GDMTag source)
        {
            GDMLocationRecord otherLoc = (source as GDMLocationRecord);
            if (otherLoc == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherLoc);

            fLocationName = otherLoc.fLocationName;
            fMap.Assign(otherLoc.fMap);
        }

        public override void Clear()
        {
            base.Clear();

            fLocationName = string.Empty;
            fMap.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fLocationName) && fMap.IsEmpty();
        }

        // TODO: connect to use
        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMLocationRecord otherLoc = tag as GDMLocationRecord;
            if (otherLoc == null) return 0.0f;

            float match = GetStrMatch(LocationName, otherLoc.LocationName, matchParams);
            return match;
        }
    }
}
