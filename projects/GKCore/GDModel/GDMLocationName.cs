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
    public sealed class GDMLocationName : GDMValueTag, IGDMStructWithDate
    {
        private string fAbbreviation;
        private readonly GDMDateValue fDate;
        private GDMLanguageID fLanguage;

        public string Abbreviation
        {
            get { return fAbbreviation; }
            set { fAbbreviation = value; }
        }

        public GDMDateValue Date
        {
            get { return fDate; }
        }

        public GDMLanguageID Language
        {
            get { return fLanguage; }
            set { fLanguage = value; }
        }

        public GDMLocationName()
        {
            SetName(GEDCOMTagType.NAME);

            fDate = new GDMDateValue();
        }

        public GDMLocationName(string strValue, GDMCustomDate date) : this()
        {
            StringValue = strValue;
            fDate.SetRawData(date);
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
            var sourceObj = (source as GDMLocationName);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

            base.Assign(sourceObj);
            fAbbreviation = sourceObj.fAbbreviation;
            fDate.Assign(sourceObj.fDate);
            fLanguage = sourceObj.fLanguage;
        }

        public override void Clear()
        {
            base.Clear();
            fAbbreviation = string.Empty;
            fDate.Clear();
            fLanguage = GDMLanguageID.Unknown;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fDate.IsEmpty() && string.IsNullOrEmpty(fAbbreviation) && (fLanguage == GDMLanguageID.Unknown);
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
            hashCode.Add(fLanguage);
        }
    }
}
