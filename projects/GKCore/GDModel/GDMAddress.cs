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
using BSLib;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public sealed class GDMAddress : GDMTag
    {
        private GDMList<GDMTag> fPhoneList;
        private GDMList<GDMTag> fEmailList;
        private GDMList<GDMTag> fFaxList;
        private GDMList<GDMTag> fWWWList;


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

        public GDMList<GDMTag> PhoneNumbers
        {
            get { return fPhoneList; }
        }

        public GDMList<GDMTag> EmailAddresses
        {
            get { return fEmailList; }
        }

        public GDMList<GDMTag> FaxNumbers
        {
            get { return fFaxList; }
        }

        public GDMList<GDMTag> WebPages
        {
            get { return fWWWList; }
        }


        public new static GDMTag Create(GDMObject owner, string tagName, string tagValue)
        {
            return new GDMAddress(owner, tagName, tagValue);
        }

        public GDMAddress(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.ADDR);

            fPhoneList = new GDMList<GDMTag>(this);
            fEmailList = new GDMList<GDMTag>(this);
            fFaxList = new GDMList<GDMTag>(this);
            fWWWList = new GDMList<GDMTag>(this);
        }

        public GDMAddress(GDMObject owner, string tagName, string tagValue) : this(owner)
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

        public override void Assign(GDMTag source)
        {
            GDMAddress otherAddr = source as GDMAddress;
            if (otherAddr == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            AssignList(otherAddr.fPhoneList, fPhoneList);
            AssignList(otherAddr.fEmailList, fEmailList);
            AssignList(otherAddr.fFaxList, fFaxList);
            AssignList(otherAddr.fWWWList, fWWWList);
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

        public GDMTag AddEmailAddress(string value)
        {
            return fEmailList.Add(new GDMTag(this, GEDCOMTagType.EMAIL, value));
        }

        public GDMTag AddFaxNumber(string value)
        {
            return fFaxList.Add(new GDMTag(this, GEDCOMTagType.FAX, value));
        }

        public GDMTag AddPhoneNumber(string value)
        {
            return fPhoneList.Add(new GDMTag(this, GEDCOMTagType.PHON, value));
        }

        public GDMTag AddWebPage(string value)
        {
            return fWWWList.Add(new GDMTag(this, GEDCOMTagType.WWW, value));
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
