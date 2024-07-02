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
    public sealed class GDMLocationType : GDMValueTag
    {
        private string fAbbreviation;
        private readonly GDMDateValue fDate;

        public string Abbreviation
        {
            get { return fAbbreviation; }
            set { fAbbreviation = value; }
        }

        public GDMDateValue Date
        {
            get { return fDate; }
        }

        public GDMLocationType()
        {
            SetName(GEDCOMTagType.TYPE);

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
            var sourceObj = (source as GDMLocationType);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);
            fAbbreviation = sourceObj.fAbbreviation;
            fDate.Assign(sourceObj.fDate);
        }

        public override void Clear()
        {
            base.Clear();
            fAbbreviation = string.Empty;
            fDate.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fDate.IsEmpty() && string.IsNullOrEmpty(fAbbreviation);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fDate.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fAbbreviation);
            hashCode.Add(fDate);
        }
    }
}
