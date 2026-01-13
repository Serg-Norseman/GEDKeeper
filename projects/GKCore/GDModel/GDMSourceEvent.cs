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
    public sealed class GDMSourceEvent : GDMValueTag, IGDMStructWithPlace
    {
        private GDMDatePeriod fDate;
        private GDMPlace fPlace;


        public GDMDatePeriod Date
        {
            get { return fDate; }
        }

        public bool HasPlace
        {
            get { return fPlace != null && !fPlace.IsEmpty(); }
        }

        public GDMPlace Place
        {
            get {
                if (fPlace == null) {
                    fPlace = new GDMPlace();
                }

                return fPlace;
            }
        }


        public GDMSourceEvent()
        {
            SetName(GEDCOMTagType.EVEN);

            fDate = new GDMDatePeriod();
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fDate.TrimExcess();
            if (fPlace != null) fPlace.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();

            fDate.Clear();
            if (fPlace != null) fPlace.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fDate.IsEmpty() &&
                (fPlace == null || fPlace.IsEmpty());
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            if (fPlace != null) fPlace.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fDate);
            hashCode.Add(fPlace);
        }
    }
}
