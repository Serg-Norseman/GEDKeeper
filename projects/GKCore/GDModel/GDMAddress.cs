﻿/*
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
        private StringList fLines;
        private string fAddressLine1;
        private string fAddressLine2;
        private string fAddressLine3;
        private string fAddressCity;
        private string fAddressState;
        private string fAddressPostalCode;
        private string fAddressCountry;
        private GDMList<GDMTag> fPhoneList;
        private GDMList<GDMTag> fEmailList;
        private GDMList<GDMTag> fFaxList;
        private GDMList<GDMTag> fWWWList;


        public StringList Lines
        {
            get { return fLines; }
        }

        public string AddressLine1
        {
            get { return fAddressLine1; }
            set { fAddressLine1 = value; }
        }

        public string AddressLine2
        {
            get { return fAddressLine2; }
            set { fAddressLine2 = value; }
        }

        public string AddressLine3
        {
            get { return fAddressLine3; }
            set { fAddressLine3 = value; }
        }

        public string AddressCity
        {
            get { return fAddressCity; }
            set { fAddressCity = value; }
        }

        public string AddressState
        {
            get { return fAddressState; }
            set { fAddressState = value; }
        }

        public string AddressPostalCode
        {
            get { return fAddressPostalCode; }
            set { fAddressPostalCode = value; }
        }

        public string AddressCountry
        {
            get { return fAddressCountry; }
            set { fAddressCountry = value; }
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


        public GDMAddress(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.ADDR);

            fLines = new StringList();
            fAddressLine1 = string.Empty;
            fAddressLine2 = string.Empty;
            fAddressLine3 = string.Empty;
            fAddressCity = string.Empty;
            fAddressState = string.Empty;
            fAddressPostalCode = string.Empty;
            fAddressCountry = string.Empty;

            fPhoneList = new GDMList<GDMTag>(this);
            fEmailList = new GDMList<GDMTag>(this);
            fFaxList = new GDMList<GDMTag>(this);
            fWWWList = new GDMList<GDMTag>(this);
        }

        public GDMAddress(GDMObject owner, int tagId, string tagValue) : this(owner)
        {
            SetNameValue(tagId, tagValue);
        }

        public new static GDMTag Create(GDMObject owner, int tagId, string tagValue)
        {
            return new GDMAddress(owner, tagId, tagValue);
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

            fLines.Assign(otherAddr.fLines);
            fAddressLine1 = otherAddr.fAddressLine1;
            fAddressLine2 = otherAddr.fAddressLine2;
            fAddressLine3 = otherAddr.fAddressLine3;
            fAddressCity = otherAddr.fAddressCity;
            fAddressState = otherAddr.fAddressState;
            fAddressPostalCode = otherAddr.fAddressPostalCode;
            fAddressCountry = otherAddr.fAddressCountry;

            AssignList(otherAddr.fPhoneList, fPhoneList);
            AssignList(otherAddr.fEmailList, fEmailList);
            AssignList(otherAddr.fFaxList, fFaxList);
            AssignList(otherAddr.fWWWList, fWWWList);
        }

        public override void Clear()
        {
            base.Clear();

            fLines.Clear();
            fAddressLine1 = string.Empty;
            fAddressLine2 = string.Empty;
            fAddressLine3 = string.Empty;
            fAddressCity = string.Empty;
            fAddressState = string.Empty;
            fAddressPostalCode = string.Empty;
            fAddressCountry = string.Empty;

            fPhoneList.Clear();
            fEmailList.Clear();
            fFaxList.Clear();
            fWWWList.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fLines.IsEmpty() &&
                string.IsNullOrEmpty(fAddressLine1) && string.IsNullOrEmpty(fAddressLine2) && string.IsNullOrEmpty(fAddressLine3) && 
                string.IsNullOrEmpty(fAddressCity) && string.IsNullOrEmpty(fAddressState) && string.IsNullOrEmpty(fAddressCountry) && 
                string.IsNullOrEmpty(fAddressPostalCode) &&
                fPhoneList.Count == 0 && fEmailList.Count == 0 && fFaxList.Count == 0 && fWWWList.Count == 0;
        }

        protected override string GetStringValue()
        {
            return (fLines.Count > 0) ? fLines[0] : string.Empty;
        }

        public override string ParseString(string strValue)
        {
            fLines.Text = strValue;
            return string.Empty;
        }

        public GDMTag AddEmailAddress(string value)
        {
            return fEmailList.Add(new GDMTag(this, (int)GEDCOMTagType.EMAIL, value));
        }

        public GDMTag AddFaxNumber(string value)
        {
            return fFaxList.Add(new GDMTag(this, (int)GEDCOMTagType.FAX, value));
        }

        public GDMTag AddPhoneNumber(string value)
        {
            return fPhoneList.Add(new GDMTag(this, (int)GEDCOMTagType.PHON, value));
        }

        public GDMTag AddWebPage(string value)
        {
            return fWWWList.Add(new GDMTag(this, (int)GEDCOMTagType.WWW, value));
        }

        public void SetAddressText(string value)
        {
            fLines.Text = value;
        }

        public void SetAddressArray(string[] value)
        {
            fLines.Clear();
            fLines.AddStrings(value);
        }
    }
}
