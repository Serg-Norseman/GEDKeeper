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
