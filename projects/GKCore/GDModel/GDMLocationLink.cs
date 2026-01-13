/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GDMLocationRelationship
    {
        Political, Religious, Geographical, Cultural
    }


    public sealed class GDMLocationLink : GDMPointer, IGDMStructWithDate
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
