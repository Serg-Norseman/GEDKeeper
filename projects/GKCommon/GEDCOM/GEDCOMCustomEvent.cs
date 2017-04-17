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

using System.IO;

namespace GKCommon.GEDCOM
{
    public abstract class GEDCOMCustomEvent : GEDCOMTagWithLists
    {
        public string Classification
        {
            get { return GetTagStringValue("TYPE"); }
            set { SetTagStringValue("TYPE", value); }
        }

        public string Agency
        {
            get { return GetTagStringValue("AGNC"); }
            set { SetTagStringValue("AGNC", value); }
        }

        public string ReligiousAffilation
        {
            get { return GetTagStringValue("RELI"); }
            set { SetTagStringValue("RELI", value); }
        }

        public string Cause
        {
            get { return GetTagStringValue("CAUS"); }
            set { SetTagStringValue("CAUS", value); }
        }

        public GEDCOMPlace Place
        {
            get { return TagClass("PLAC", GEDCOMPlace.Create) as GEDCOMPlace; }
        }

        public GEDCOMAddress Address
        {
            get { return TagClass("ADDR", GEDCOMAddress.Create) as GEDCOMAddress; }
        }

        public GEDCOMDateValue Date
        {
            get { return TagClass("DATE", GEDCOMDateValue.Create) as GEDCOMDateValue; }
        }

        public GEDCOMRestriction Restriction
        {
            get { return GEDCOMUtils.GetRestrictionVal(GetTagStringValue("RESN")); }
            set { SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "PHON" || tagName == "EMAIL" || tagName == "FAX" || tagName == "WWW")
            {
                result = Address.AddTag(tagName, tagValue, tagConstructor);
            }
            else
            {
                // define "PLAC", "ADDR", "DATE" by default
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        protected GEDCOMCustomEvent(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            if (tag == null) return 0.0f;
            GEDCOMCustomEvent ev = (GEDCOMCustomEvent)tag;

            // match date
            float dateMatch = 0.0f;
            GEDCOMDateValue dtVal = this.Date;
            GEDCOMDateValue dtVal2 = ev.Date;

            if (dtVal != null && dtVal2 != null) {
                dateMatch = dtVal.IsMatch(dtVal2, matchParams);
            }

            // match location - late code-on by option implementation
            /*float locMatch = 0.0f;
			if (this.Place == null && ev.Place == null)
			{
				locMatch = 100.0f;
			}
			else if (this.Place != null && ev.Place != null)
			{
				if (this.Place.StringValue == ev.Place.StringValue)
				{
					locMatch = 100.0f;
				}
			}*/

            float match = (dateMatch); /* + locMatch) / 2.0f;*/
            return match;
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
