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
    // FIXME: possible numerous violations of the standard; need to recheck the nesting of tags
    public sealed class GEDCOMIndividualOrdinance : GEDCOMTagWithLists
    {
        public GEDCOMDateValue Date
        {
            get { return TagClass("DATE", GEDCOMDateValue.Create) as GEDCOMDateValue; }
        }

        public string TempleCode
        {
            get { return GetTagStringValue("TEMP"); }
            set { SetTagStringValue("TEMP", value); }
        }

        public GEDCOMPlace Place
        {
            get { return TagClass("PLAC", GEDCOMPlace.Create) as GEDCOMPlace; }
        }

        public GEDCOMBaptismDateStatus BaptismDateStatus
        {
            get { return GEDCOMUtils.GetBaptismDateStatusVal(GetTagStringValue("STAT")); }
            set { SetTagStringValue("STAT", GEDCOMUtils.GetBaptismDateStatusStr(value)); }
        }

        public GEDCOMDateExact BaptismChangeDate
        {
            get { return GetChangeDate(); }
        }

        public GEDCOMEndowmentDateStatus EndowmentDateStatus
        {
            get { return GEDCOMUtils.GetEndowmentDateStatusVal(GetTagStringValue("STAT")); }
            set { SetTagStringValue("STAT", GEDCOMUtils.GetEndowmentDateStatusStr(value)); }
        }

        public GEDCOMDateExact EndowmentChangeDate
        {
            get { return GetChangeDate(); }
        }

        public GEDCOMPointer Family
        {
            get { return TagClass("FAMC", GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMChildSealingDateStatus ChildSealingDateStatus
        {
            get { return GEDCOMUtils.GetChildSealingDateStatusVal(GetTagStringValue("STAT")); }
            set { SetTagStringValue("STAT", GEDCOMUtils.GetChildSealingDateStatusStr(value)); }
        }

        public GEDCOMDateExact ChildSealingChangeDate
        {
            get { return GetChangeDate(); }
        }


        private GEDCOMDateExact GetChangeDate()
        {
            return DateStatus.TagClass("CHAN", GEDCOMDateExact.Create) as GEDCOMDateExact;
        }

        public GEDCOMDateStatus DateStatus
        {
            get { return TagClass("STAT", GEDCOMDateStatus.Create) as GEDCOMDateStatus; }
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "STAT")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMDateStatus.Create);
            }
            else if (tagName == "FAMC")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
            }
            else
            {
                // define "DATE" by default
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public GEDCOMIndividualOrdinance(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMIndividualOrdinance(owner, parent, tagName, tagValue);
        }
    }
}
