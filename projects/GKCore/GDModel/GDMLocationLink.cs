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

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GDMLocationRelationship
    {
        Political, Religious, Geographical, Cultural
    }


    public sealed class GDMLocationLink : GDMPointer, IGDMLocationElement
    {
        private readonly GDMDateValue fDate;
        private GDMLocationRelationship fRelationship;

        public GDMDateValue Date
        {
            get { return fDate; }
        }

        public GDMLocationRelationship Relationship
        {
            get { return fRelationship; }
            set { fRelationship = value; }
        }

        public GDMLocationLink()
        {
            SetName(GEDCOMTagType._LOC);

            fDate = new GDMDateValue();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fDate.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();
            fDate.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            var sourceObj = (source as GDMLocationLink);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);
            fDate.Assign(sourceObj.fDate);
            fRelationship = sourceObj.fRelationship;
        }

        public override void Clear()
        {
            base.Clear();
            fDate.Clear();
            fRelationship = GDMLocationRelationship.Political;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fDate.IsEmpty();
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fDate.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fDate);
            hashCode.Add(fRelationship);
        }
    }
}
