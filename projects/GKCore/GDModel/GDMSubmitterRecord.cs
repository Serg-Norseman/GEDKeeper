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

namespace GDModel
{
    public sealed class GDMSubmitterRecord : GDMRecord
    {
        private GDMList<GDMLanguage> fLanguages;


        public GDMAddress Address
        {
            get { return GetTag<GDMAddress>(GEDCOMTagType.ADDR, GDMAddress.Create); }
        }

        public GDMList<GDMLanguage> Languages
        {
            get { return fLanguages; }
        }

        public new GDMPersonalName Name
        {
            get { return GetTag<GDMPersonalName>(GEDCOMTagType.NAME, GDMPersonalName.Create); }
        }

        public string RegisteredReference
        {
            get { return GetTagStringValue(GEDCOMTagType.RFN); }
            set { SetTagStringValue(GEDCOMTagType.RFN, value); }
        }


        public void SetLanguage(int index, string value)
        {
            if (index < 0) return;

            while (index >= fLanguages.Count) {
                fLanguages.Add(new GDMLanguage(this, GEDCOMTagType.LANG, ""));
            }
            fLanguages[index].StringValue = value;
        }

        public GDMSubmitterRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtSubmitter);
            SetName(GEDCOMTagType.SUBM);

            fLanguages = new GDMList<GDMLanguage>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fLanguages.Dispose();
            }
            base.Dispose(disposing);
        }

        public GDMLanguage AddLanguage(GDMLanguage value)
        {
            fLanguages.Add(value);
            return value;
        }

        public override void Clear()
        {
            base.Clear();
            fLanguages.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fLanguages.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fLanguages.ReplaceXRefs(map);
        }
    }
}
