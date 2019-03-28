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

using System;
using System.IO;
using BSLib;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMAddress : GEDCOMTag
    {
        private GEDCOMList<GEDCOMTag> fPhoneList;
        private GEDCOMList<GEDCOMTag> fEmailList;
        private GEDCOMList<GEDCOMTag> fFaxList;
        private GEDCOMList<GEDCOMTag> fWWWList;


        public StringList Address
        {
            get { return GetTagStrings(this); }
            set { SetTagStrings(this, value); }
        }

        public string AddressLine1
        {
            get { return GetTagStringValue(GEDCOMTagType.ADR1); }
            set { SetTagStringValue(GEDCOMTagType.ADR1, value); }
        }

        public string AddressLine2
        {
            get { return GetTagStringValue(GEDCOMTagType.ADR2); }
            set { SetTagStringValue(GEDCOMTagType.ADR2, value); }
        }

        public string AddressLine3
        {
            get { return GetTagStringValue(GEDCOMTagType.ADR3); }
            set { SetTagStringValue(GEDCOMTagType.ADR3, value); }
        }

        public string AddressCity
        {
            get { return GetTagStringValue(GEDCOMTagType.CITY); }
            set { SetTagStringValue(GEDCOMTagType.CITY, value); }
        }

        public string AddressState
        {
            get { return GetTagStringValue(GEDCOMTagType.STAE); }
            set { SetTagStringValue(GEDCOMTagType.STAE, value); }
        }

        public string AddressPostalCode
        {
            get { return GetTagStringValue(GEDCOMTagType.POST); }
            set { SetTagStringValue(GEDCOMTagType.POST, value); }
        }

        public string AddressCountry
        {
            get { return GetTagStringValue(GEDCOMTagType.CTRY); }
            set { SetTagStringValue(GEDCOMTagType.CTRY, value); }
        }

        public GEDCOMList<GEDCOMTag> PhoneNumbers
        {
            get { return fPhoneList; }
        }

        public GEDCOMList<GEDCOMTag> EmailAddresses
        {
            get { return fEmailList; }
        }

        public GEDCOMList<GEDCOMTag> FaxNumbers
        {
            get { return fFaxList; }
        }

        public GEDCOMList<GEDCOMTag> WebPages
        {
            get { return fWWWList; }
        }


        public new static GEDCOMTag Create(GEDCOMObject owner, string tagName, string tagValue)
        {
            return new GEDCOMAddress(owner, tagName, tagValue);
        }

        public GEDCOMAddress(GEDCOMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.ADDR);

            fPhoneList = new GEDCOMList<GEDCOMTag>(this);
            fEmailList = new GEDCOMList<GEDCOMTag>(this);
            fFaxList = new GEDCOMList<GEDCOMTag>(this);
            fWWWList = new GEDCOMList<GEDCOMTag>(this);
        }

        public GEDCOMAddress(GEDCOMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fPhoneList.Dispose();
                fEmailList.Dispose();
                fFaxList.Dispose();
                fWWWList.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            fPhoneList.SaveToStream(stream);
            fEmailList.SaveToStream(stream);
            fFaxList.SaveToStream(stream);
            fWWWList.SaveToStream(stream);
        }

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMAddress otherAddr = source as GEDCOMAddress;
            if (otherAddr == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            AssignList(otherAddr.fPhoneList, fPhoneList);
            AssignList(otherAddr.fEmailList, fEmailList);
            AssignList(otherAddr.fFaxList, fFaxList);
            AssignList(otherAddr.fWWWList, fWWWList);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.PHON) {
                result = (fPhoneList.Add(new GEDCOMTag(this, tagName, tagValue)));
                result.SetLevel(Level);
            } else if (tagName == GEDCOMTagType.EMAIL) {
                result = (fEmailList.Add(new GEDCOMTag(this, tagName, tagValue)));
                result.SetLevel(Level);
            } else if (tagName == GEDCOMTagType.FAX) {
                result = (fFaxList.Add(new GEDCOMTag(this, tagName, tagValue)));
                result.SetLevel(Level);
            } else if (tagName == GEDCOMTagType.WWW) {
                result = (fWWWList.Add(new GEDCOMTag(this, tagName, tagValue)));
                result.SetLevel(Level);
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            fPhoneList.Clear();
            fEmailList.Clear();
            fFaxList.Clear();
            fWWWList.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fPhoneList.Count == 0 && fEmailList.Count == 0 && fFaxList.Count == 0 && fWWWList.Count == 0;
        }

        public void AddEmailAddress(string value)
        {
            GEDCOMTag tag = fEmailList.Add(new GEDCOMTag(this, GEDCOMTagType.EMAIL, value));
            tag.SetLevel(Level);
        }

        public void AddFaxNumber(string value)
        {
            GEDCOMTag tag = fFaxList.Add(new GEDCOMTag(this, GEDCOMTagType.FAX, value));
            tag.SetLevel(Level);
        }

        public void AddPhoneNumber(string value)
        {
            GEDCOMTag tag = fPhoneList.Add(new GEDCOMTag(this, GEDCOMTagType.PHON, value));
            tag.SetLevel(Level);
        }

        public void AddWebPage(string value)
        {
            GEDCOMTag tag = fWWWList.Add(new GEDCOMTag(this, GEDCOMTagType.WWW, value));
            tag.SetLevel(Level);
        }

        public void SetAddressText(string value)
        {
            using (StringList sl = new StringList(value)) {
                Address = sl;
            }
        }

        public void SetAddressArray(string[] value)
        {
            SetTagStrings(this, value);
        }
    }
}
