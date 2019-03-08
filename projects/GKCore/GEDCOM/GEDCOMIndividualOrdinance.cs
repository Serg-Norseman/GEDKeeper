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
    public sealed class GEDCOMIndividualOrdinance : GEDCOMTagWithLists
    {
        public GEDCOMDateValue Date
        {
            get { return TagClass(GEDCOMTagType.DATE, GEDCOMDateValue.Create) as GEDCOMDateValue; }
        }

        public string TempleCode
        {
            get { return GetTagStringValue(GEDCOMTagType.TEMP); }
            set { SetTagStringValue(GEDCOMTagType.TEMP, value); }
        }

        public GEDCOMPlace Place
        {
            get { return TagClass(GEDCOMTagType.PLAC, GEDCOMPlace.Create) as GEDCOMPlace; }
        }

        public GEDCOMBaptismDateStatus BaptismDateStatus
        {
            get { return GEDCOMUtils.GetBaptismDateStatusVal(GetTagStringValue(GEDCOMTagType.STAT)); }
            set { SetTagStringValue(GEDCOMTagType.STAT, GEDCOMUtils.GetBaptismDateStatusStr(value)); }
        }

        public GEDCOMDate BaptismChangeDate
        {
            get { return GetChangeDate(); }
        }

        public GEDCOMEndowmentDateStatus EndowmentDateStatus
        {
            get { return GEDCOMUtils.GetEndowmentDateStatusVal(GetTagStringValue(GEDCOMTagType.STAT)); }
            set { SetTagStringValue(GEDCOMTagType.STAT, GEDCOMUtils.GetEndowmentDateStatusStr(value)); }
        }

        public GEDCOMDate EndowmentChangeDate
        {
            get { return GetChangeDate(); }
        }

        public GEDCOMPointer Family
        {
            get { return TagClass(GEDCOMTagType.FAMC, GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMChildSealingDateStatus ChildSealingDateStatus
        {
            get { return GEDCOMUtils.GetChildSealingDateStatusVal(GetTagStringValue(GEDCOMTagType.STAT)); }
            set { SetTagStringValue(GEDCOMTagType.STAT, GEDCOMUtils.GetChildSealingDateStatusStr(value)); }
        }

        public GEDCOMDate ChildSealingChangeDate
        {
            get { return GetChangeDate(); }
        }

        private GEDCOMDate GetChangeDate()
        {
            return DateStatus.TagClass(GEDCOMTagType.DATE, GEDCOMDate.Create) as GEDCOMDate;
        }

        public GEDCOMDateStatus DateStatus
        {
            get { return TagClass(GEDCOMTagType.STAT, GEDCOMDateStatus.Create) as GEDCOMDateStatus; }
        }


        public GEDCOMIndividualOrdinance(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
        }

        public GEDCOMIndividualOrdinance(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : this(owner, parent)
        {
            SetNameValue(tagName, tagValue);
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMIndividualOrdinance(owner, parent, tagName, tagValue);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.STAT) {
                result = base.AddTag(tagName, tagValue, GEDCOMDateStatus.Create);
            } else {
                // define 'DATE', 'FAMC' by default
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }
    }
}
