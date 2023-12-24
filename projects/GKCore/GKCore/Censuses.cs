/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.IO;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;

namespace GKCore
{
    public class Census
    {
        private string fCountry;
        private string fDate;
        private string fName;


        public string Country
        {
            get { return fCountry; }
            set { fCountry = value; }
        }

        public string Date
        {
            get { return fDate; }
            set { fDate = value; }
        }

        public string Name
        {
            get { return fName; }
            set { fName = value; }
        }


        public GDMDateValue GDMDate { get; internal set; }


        public Census()
        {
        }

        public Census(string country, string date, string name)
        {
            fCountry = country;
            fDate = date;
            fName = name;
        }

        public override string ToString()
        {
            return fName;
        }
    }


    internal class CensusesList
    {
        public Census[] Censuses { get; set; }


        public CensusesList()
        {
            Censuses = new Census[0];
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class Censuses
    {
        private static Censuses fInstance = null;

        public static Censuses Instance
        {
            get {
                if (fInstance == null) fInstance = new Censuses();
                return fInstance;
            }
        }


        private CensusesList fCensuses;

        private Censuses()
        {
            fCensuses = new CensusesList();
        }

        public void Load(string fileName)
        {
            fCensuses.Censuses.Clear();
            if (!File.Exists(fileName)) return;

            try {
                // loading database
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    fCensuses = YamlHelper.Deserialize<CensusesList>(content);
                }

                // processing dates
                for (int i = 0; i < fCensuses.Censuses.Length; i++) {
                    var census = fCensuses.Censuses[i];
                    census.GDMDate = new GDMDateValue();
                    GEDCOMUtils.ParseDateValue(null, census.GDMDate, census.Date);
                }
            } catch (Exception ex) {
                Logger.WriteError("Censuses.Load()", ex);
            }
        }

        // FIXME: The function returns complete nonsense!
        public IEnumerable<Census> FindMatchedIndividualCensuses(GDMCustomEvent evtBirth, GDMCustomEvent evtDeath)
        {
            var result = new List<Census>();
            // Expected a sorted list of censuses
            foreach (var census in fCensuses.Censuses) {
                // TODO: indistinct compare country and pieces of evt.Place
                // GKData.PROVED_LIFE_LENGTH

                // In the case of the fact of death, only the next census should be displayed,
                // in the case of the fact of birth - the next one and the rest either until the actual death or the maximum permissible age.
                bool isMatched = false;

                if (evtBirth != null && IsMatchedDate(evtBirth.Date, census.GDMDate)) {
                    result.Add(census);
                    isMatched = true;
                }

                if (evtDeath != null && IsMatchedDate(evtDeath.Date, census.GDMDate)) {
                    if (!isMatched) {
                        result.Add(census);
                    }
                    break;
                }
            }
            return result;
        }

        private static bool IsMatchedDate(GDMDateValue eventDateVal, GDMDateValue censusDateVal)
        {
            if (eventDateVal.Value == null || censusDateVal.Value == null) return false;

            GDMCustomDate eventDate;
            {
                var edRange = eventDateVal.Value as GDMDateRange;
                if (edRange != null) {
                    // BET AFT-X AND BEF-Y
                    eventDate = !edRange.After.IsEmpty() ? edRange.After : edRange.Before;
                } else {
                    var edPeriod = eventDateVal.Value as GDMDatePeriod;
                    if (edPeriod != null) {
                        // FROM X TO Y
                        eventDate = !edPeriod.DateFrom.IsEmpty() ? edPeriod.DateFrom : edPeriod.DateTo;
                    } else {
                        eventDate = eventDateVal.Value;
                    }
                }
            }

            GDMCustomDate censusDate;
            {
                var cdRange = censusDateVal.Value as GDMDateRange;
                if (cdRange != null) {
                    // BET AFT-X AND BEF-Y
                    censusDate = !cdRange.After.IsEmpty() ? cdRange.After : cdRange.Before;
                } else {
                    var cdPeriod = censusDateVal.Value as GDMDatePeriod;
                    if (cdPeriod != null) {
                        // FROM X TO Y
                        censusDate = !cdPeriod.DateFrom.IsEmpty() ? cdPeriod.DateFrom : cdPeriod.DateTo;
                    } else {
                        censusDate = censusDateVal.Value;
                    }
                }
            }

            var eventUDN = eventDate.GetUDN();
            var censusUDN = censusDate.GetUDN();

            return eventUDN.HasKnownYear() && censusUDN.HasKnownYear() && (eventUDN.CompareTo(censusUDN) <= 0);
        }
    }
}
