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
    public sealed class GEDCOMRepositoryRecord : GEDCOMRecord
    {
        public GEDCOMAddress Address
        {
            get { return GetTag(GEDCOMTagType.ADDR, GEDCOMAddress.Create) as GEDCOMAddress; }
        }

        public string RepositoryName
        {
            get { return GetTagStringValue(GEDCOMTagType.NAME); }
            set { SetTagStringValue(GEDCOMTagType.NAME, value); }
        }


        public GEDCOMRepositoryRecord(GEDCOMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtRepository);
            SetName(GEDCOMTagType.REPO);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                result = Address.AddTag(tagName, tagValue, tagConstructor);
            } else {
                // 'ADDR' defines by default
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        // TODO: connect to use
        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMRepositoryRecord otherRep = tag as GEDCOMRepositoryRecord;
            if (otherRep == null) return 0.0f;

            float match = GetStrMatch(RepositoryName, otherRep.RepositoryName, matchParams);
            return match;
        }
    }
}
