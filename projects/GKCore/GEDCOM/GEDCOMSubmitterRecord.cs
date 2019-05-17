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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMSubmitterRecord : GEDCOMRecord
    {
        private GEDCOMList<GEDCOMLanguage> fLanguages;

        public GEDCOMAddress Address
        {
            get { return GetTag(GEDCOMTagType.ADDR, GEDCOMAddress.Create) as GEDCOMAddress; }
        }

        public GEDCOMList<GEDCOMLanguage> Languages
        {
            get { return fLanguages; }
        }

        public new GEDCOMPersonalName Name
        {
            get { return GetTag(GEDCOMTagType.NAME, GEDCOMPersonalName.Create) as GEDCOMPersonalName; }
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
                fLanguages.Add(new GEDCOMLanguage(this, GEDCOMTagType.LANG, ""));
            }
            fLanguages[index].StringValue = value;
        }


        public GEDCOMSubmitterRecord(GEDCOMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtSubmitter);
            SetName(GEDCOMTagType.SUBM);

            fLanguages = new GEDCOMList<GEDCOMLanguage>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fLanguages.Dispose();
            }
            base.Dispose(disposing);
        }

        public GEDCOMLanguage AddLanguage(GEDCOMLanguage value)
        {
            fLanguages.Add(value);
            return value;
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.NAME) {
                result = base.AddTag(tagName, tagValue, GEDCOMPersonalName.Create);
            } else if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                result = Address.AddTag(tagName, tagValue, tagConstructor);
            } else if (tagName == GEDCOMTagType.LANG) {
                result = AddLanguage(new GEDCOMLanguage(this, tagName, tagValue));
            } else {
                // 'ADDR' defines by default
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
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

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fLanguages.ReplaceXRefs(map);
        }
    }
}
