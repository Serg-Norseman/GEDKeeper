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

using BSLib.Calendar;
using GKCore.Types;

namespace GKCommon.GEDCOM
{
    public abstract class GDMCustomEvent : GDMTagWithLists
    {
        public string Classification
        {
            get { return GetTagStringValue(GEDCOMTagType.TYPE); }
            set { SetTagStringValue(GEDCOMTagType.TYPE, value); }
        }

        public string Agency
        {
            get { return GetTagStringValue(GEDCOMTagType.AGNC); }
            set { SetTagStringValue(GEDCOMTagType.AGNC, value); }
        }

        public string ReligiousAffilation
        {
            get { return GetTagStringValue(GEDCOMTagType.RELI); }
            set { SetTagStringValue(GEDCOMTagType.RELI, value); }
        }

        public string Cause
        {
            get { return GetTagStringValue(GEDCOMTagType.CAUS); }
            set { SetTagStringValue(GEDCOMTagType.CAUS, value); }
        }

        public GDMPlace Place
        {
            get { return GetTag<GDMPlace>(GEDCOMTagType.PLAC, GDMPlace.Create); }
        }

        public GDMAddress Address
        {
            get { return GetTag<GDMAddress>(GEDCOMTagType.ADDR, GDMAddress.Create); }
        }

        public GDMDateValue Date
        {
            get { return GetTag<GDMDateValue>(GEDCOMTagType.DATE, GDMDateValue.Create); }
        }

        public GEDCOMRestriction Restriction
        {
            get { return GEDCOMUtils.GetRestrictionVal(GetTagStringValue(GEDCOMTagType.RESN)); }
            set { SetTagStringValue(GEDCOMTagType.RESN, GEDCOMUtils.GetRestrictionStr(value)); }
        }


        protected GDMCustomEvent(GDMObject owner) : base(owner)
        {
        }

        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            if (tag == null) return 0.0f;
            GDMCustomEvent ev = (GDMCustomEvent)tag;

            // match date
            float dateMatch = 0.0f;
            float locMatch = 0.0f;
            int matches = 0;

            GDMDateValue dtVal = this.Date;
            GDMDateValue dtVal2 = ev.Date;

            matches += 1;
            if (dtVal != null && dtVal2 != null) {
                dateMatch = dtVal.IsMatch(dtVal2, matchParams);
            }

            // match location - late code-on by option implementation
            if (matchParams.CheckEventPlaces) {
                matches += 1;

                if (this.Place == null && ev.Place == null) {
                    locMatch = 100.0f;
                } else if (this.Place != null && ev.Place != null && this.Place.StringValue == ev.Place.StringValue) {
                    locMatch = 100.0f;
                }
            }

            float match = (dateMatch + locMatch) / matches;
            return match;
        }

        public UDN GetUDN()
        {
            return Date.GetUDN();
        }

        /// <summary>
        /// In the historical chronology of the year 0 does not exist.
        /// Therefore, the digit 0 in the year value can be used as a sign of lack or error.
        /// ChronologicalYear - introduced for the purposes of uniform chronology years in the Gregorian calendar.
        /// Is estimated from -4714 BC to 3268 AD.
        /// </summary>
        /// <returns>chronological year</returns>
        public int GetChronologicalYear()
        {
            return Date.GetChronologicalYear();
        }
    }
}
