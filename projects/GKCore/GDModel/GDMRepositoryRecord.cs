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

using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMRepositoryRecord : GDMRecord
    {
        public GDMAddress Address
        {
            get { return GetTag<GDMAddress>(GEDCOMTagType.ADDR, GDMAddress.Create); }
        }

        public string RepositoryName
        {
            get { return GetTagStringValue(GEDCOMTagType.NAME); }
            set { SetTagStringValue(GEDCOMTagType.NAME, value); }
        }


        public GDMRepositoryRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtRepository);
            SetName(GEDCOMTagType.REPO);
        }

        // TODO: connect to use
        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMRepositoryRecord otherRep = tag as GDMRepositoryRecord;
            if (otherRep == null) return 0.0f;

            float match = GetStrMatch(RepositoryName, otherRep.RepositoryName, matchParams);
            return match;
        }
    }
}
