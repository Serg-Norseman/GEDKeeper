/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
            get { return base.TagClass("ADDR", GEDCOMAddress.Create) as GEDCOMAddress; }
        }

        public GEDCOMList<GEDCOMLanguage> Languages
        {
            get { return this.fLanguages; }
        }

        public new GEDCOMPersonalName Name
        {
            get { return base.TagClass("NAME", GEDCOMPersonalName.Create) as GEDCOMPersonalName; }
        }

        public string RegisteredReference
        {
            get { return base.GetTagStringValue("RFN"); }
            set { base.SetTagStringValue("RFN", value); }
        }

        public void SetLanguage(int index, string value)
        {
            if (index >= 0)
            {
                while (index >= this.fLanguages.Count)
                {
                    this.fLanguages.Add(new GEDCOMLanguage(base.Owner, this, "LANG", ""));
                }
                this.fLanguages[index].StringValue = value;
            }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetRecordType(GEDCOMRecordType.rtSubmitter);
            base.SetName("SUBM");

            this.fLanguages = new GEDCOMList<GEDCOMLanguage>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fLanguages.Dispose();
            }
            base.Dispose(disposing);
        }

        public GEDCOMLanguage AddLanguage(GEDCOMLanguage value)
        {
            this.fLanguages.Add(value);
            return value;
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "NAME")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMPersonalName.Create);
            }
            else if (tagName == "PHON" || tagName == "EMAIL" || tagName == "FAX" || tagName == "WWW")
            {
                result = this.Address.AddTag(tagName, tagValue, tagConstructor);
            }
            else if (tagName == "LANG")
            {
                result = this.AddLanguage(new GEDCOMLanguage(base.Owner, this, tagName, tagValue));
            }
            else
            {
                // "ADDR" defines by default
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            this.fLanguages.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (this.fLanguages.Count == 0);
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            this.fLanguages.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            this.fLanguages.ResetOwner(newOwner);
        }

        public GEDCOMSubmitterRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMSubmitterRecord(owner, parent, tagName, tagValue);
        }
    }
}
