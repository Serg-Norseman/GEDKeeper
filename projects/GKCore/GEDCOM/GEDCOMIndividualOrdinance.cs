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
    public class GEDCOMIndividualOrdinance : GEDCOMTagWithLists
    {
        public GEDCOMDateValue Date
        {
            get { return GetTag(GEDCOMTagType.DATE, GEDCOMDateValue.Create) as GEDCOMDateValue; }
        }

        public string TempleCode
        {
            get { return GetTagStringValue(GEDCOMTagType.TEMP); }
            set { SetTagStringValue(GEDCOMTagType.TEMP, value); }
        }

        public GEDCOMPlace Place
        {
            get { return GetTag(GEDCOMTagType.PLAC, GEDCOMPlace.Create) as GEDCOMPlace; }
        }

        public GEDCOMBaptismDateStatus BaptismDateStatus
        {
            get { return GEDCOMUtils.GetBaptismDateStatusVal(DateStatus.StringValue); }
            set { DateStatus.StringValue = GEDCOMUtils.GetBaptismDateStatusStr(value); }
        }

        public GEDCOMEndowmentDateStatus EndowmentDateStatus
        {
            get { return GEDCOMUtils.GetEndowmentDateStatusVal(DateStatus.StringValue); }
            set { DateStatus.StringValue = GEDCOMUtils.GetEndowmentDateStatusStr(value); }
        }

        public GEDCOMPointer Family
        {
            get { return GetTag(GEDCOMTagType.FAMC, GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMChildSealingDateStatus ChildSealingDateStatus
        {
            get { return GEDCOMUtils.GetChildSealingDateStatusVal(DateStatus.StringValue); }
            set { DateStatus.StringValue = GEDCOMUtils.GetChildSealingDateStatusStr(value); }
        }

        public GEDCOMDateStatus DateStatus
        {
            get { return GetTag(GEDCOMTagType.STAT, GEDCOMDateStatus.Create) as GEDCOMDateStatus; }
        }


        public GEDCOMIndividualOrdinance(GEDCOMObject owner) : base(owner)
        {
        }

        public GEDCOMIndividualOrdinance(GEDCOMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public new static GEDCOMTag Create(GEDCOMObject owner, string tagName, string tagValue)
        {
            return new GEDCOMIndividualOrdinance(owner, tagName, tagValue);
        }
    }
}
