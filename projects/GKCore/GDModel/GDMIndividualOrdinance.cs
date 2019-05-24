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
    public enum GDMBaptismDateStatus
    {
        bdsNone,
        bdsChild,
        bdsCompleted,
        bdsExcluded,
        bdsPre1970,
        bdsStillborn,
        bdsSubmitted,
        bdsUncleared
    }


    public enum GDMEndowmentDateStatus
    {
        edsNone,
        edsChild,
        edsCompleted,
        edsExcluded,
        edsInfant,
        edsPre1970,
        edsStillborn,
        edsSubmitted,
        edsUncleared
    }


    public enum GDMChildSealingDateStatus
    {
        cdsNone,
        cdsBIC,
        cdsExcluded,
        cdsPre1970,
        cdsStillborn,
        cdsSubmitted,
        cdsUncleared
    }


    public class GDMIndividualOrdinance : GDMTagWithLists
    {
        public GDMDateValue Date
        {
            get { return GetTag<GDMDateValue>(GEDCOMTagType.DATE, GDMDateValue.Create); }
        }

        public string TempleCode
        {
            get { return GetTagStringValue(GEDCOMTagType.TEMP); }
            set { SetTagStringValue(GEDCOMTagType.TEMP, value); }
        }

        public GDMPlace Place
        {
            get { return GetTag<GDMPlace>(GEDCOMTagType.PLAC, GDMPlace.Create); }
        }

        public GDMBaptismDateStatus BaptismDateStatus
        {
            get { return GEDCOMUtils.GetBaptismDateStatusVal(DateStatus.StringValue); }
            set { DateStatus.StringValue = GEDCOMUtils.GetBaptismDateStatusStr(value); }
        }

        public GDMEndowmentDateStatus EndowmentDateStatus
        {
            get { return GEDCOMUtils.GetEndowmentDateStatusVal(DateStatus.StringValue); }
            set { DateStatus.StringValue = GEDCOMUtils.GetEndowmentDateStatusStr(value); }
        }

        public GDMPointer Family
        {
            get { return GetTag<GDMPointer>(GEDCOMTagType.FAMC, GDMPointer.Create); }
        }

        public GDMChildSealingDateStatus ChildSealingDateStatus
        {
            get { return GEDCOMUtils.GetChildSealingDateStatusVal(DateStatus.StringValue); }
            set { DateStatus.StringValue = GEDCOMUtils.GetChildSealingDateStatusStr(value); }
        }

        public GDMDateStatus DateStatus
        {
            get { return GetTag<GDMDateStatus>(GEDCOMTagType.STAT, GDMDateStatus.Create); }
        }


        public GDMIndividualOrdinance(GDMObject owner) : base(owner)
        {
        }

        public GDMIndividualOrdinance(GDMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public new static GDMTag Create(GDMObject owner, string tagName, string tagValue)
        {
            return new GDMIndividualOrdinance(owner, tagName, tagValue);
        }
    }
}
