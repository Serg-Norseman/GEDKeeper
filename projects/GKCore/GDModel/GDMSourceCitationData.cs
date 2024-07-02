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
