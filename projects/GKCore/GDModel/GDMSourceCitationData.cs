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
    public sealed class GDMSourceCitationData : GDMTag
    {
        private readonly GDMDateValue fDate;
        private readonly GDMTextTag fText;


        public GDMDateValue Date
        {
            get { return fDate; }
        }

        public GDMTextTag Text
        {
            get { return fText; }
        }


        public GDMSourceCitationData()
        {
            SetName(GEDCOMTagType.DATA);

            fDate = new GDMDateValue();
            fText = new GDMTextTag((int)GEDCOMTagType.TEXT);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fDate.Dispose();
                fText.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fDate.TrimExcess();
            fText.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMSourceCitationData otherObj = (source as GDMSourceCitationData);
            if (otherObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherObj);

            fDate.Assign(otherObj.fDate);
            fText.Assign(otherObj.fText);
        }

        public override void Clear()
        {
            base.Clear();
            fDate.Clear();
            fText.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fDate.IsEmpty() && fText.IsEmpty();
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fDate);
            hashCode.Add(fText);
        }
    }
}
