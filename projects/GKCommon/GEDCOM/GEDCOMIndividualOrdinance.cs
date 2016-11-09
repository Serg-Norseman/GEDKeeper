/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
            get { return base.TagClass("DATE", GEDCOMDateValue.Create) as GEDCOMDateValue; }
        }

        public string TempleCode
        {
            get { return base.GetTagStringValue("TEMP"); }
            set { base.SetTagStringValue("TEMP", value); }
        }

        public GEDCOMPlace Place
        {
            get { return base.TagClass("PLAC", GEDCOMPlace.Create) as GEDCOMPlace; }
        }

        public GEDCOMBaptismDateStatus BaptismDateStatus
        {
            get { return GEDCOMUtils.GetBaptismDateStatusVal(base.GetTagStringValue("STAT")); }
            set { base.SetTagStringValue("STAT", GEDCOMUtils.GetBaptismDateStatusStr(value)); }
        }

        public GEDCOMDateExact BaptismChangeDate
        {
            get { return this.GetChangeDate(); }
        }

        public GEDCOMEndowmentDateStatus EndowmentDateStatus
        {
            get { return GEDCOMUtils.GetEndowmentDateStatusVal(base.GetTagStringValue("STAT")); }
            set { base.SetTagStringValue("STAT", GEDCOMUtils.GetEndowmentDateStatusStr(value)); }
        }

        public GEDCOMDateExact EndowmentChangeDate
        {
            get { return this.GetChangeDate(); }
        }

        public GEDCOMPointer Family
        {
            get { return base.TagClass("FAMC", GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMChildSealingDateStatus ChildSealingDateStatus
        {
            get { return GEDCOMUtils.GetChildSealingDateStatusVal(base.GetTagStringValue("STAT")); }
            set { base.SetTagStringValue("STAT", GEDCOMUtils.GetChildSealingDateStatusStr(value)); }
        }

        public GEDCOMDateExact ChildSealingChangeDate
        {
            get { return this.GetChangeDate(); }
        }


        private GEDCOMDateExact GetChangeDate()
        {
            return this.DateStatus.TagClass("CHAN", GEDCOMDateExact.Create) as GEDCOMDateExact;
        }

        public GEDCOMDateStatus DateStatus
        {
            get { return base.TagClass("STAT", GEDCOMDateStatus.Create) as GEDCOMDateStatus; }
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
