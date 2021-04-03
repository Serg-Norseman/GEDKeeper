/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
    public sealed class GDMRepositoryRecord : GDMRecord
    {
        private GDMAddress fAddress;
        private string fRepositoryName;


        public GDMAddress Address
        {
            get { return fAddress; }
        }

        public string RepositoryName
        {
            get { return fRepositoryName; }
            set { fRepositoryName = value; }
        }


        public GDMRepositoryRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.REPO);

            fAddress = new GDMAddress(this);
            fRepositoryName = string.Empty;
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fAddress.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMRepositoryRecord otherRepo = (source as GDMRepositoryRecord);
            if (otherRepo == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherRepo);

            fAddress.Assign(otherRepo.fAddress);
            fRepositoryName = otherRepo.fRepositoryName;
        }

        public override void Clear()
        {
            base.Clear();

            fAddress.Clear();
            fRepositoryName = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fAddress.IsEmpty() && string.IsNullOrEmpty(fRepositoryName);
        }

        // TODO: connect to use
        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMRepositoryRecord otherRep = tag as GDMRepositoryRecord;
            if (otherRep == null) return 0.0f;

            float match = GetStrMatch(RepositoryName, otherRep.RepositoryName, matchParams);
            return match;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fAddress.ReplaceXRefs(map);
        }
    }
}
