/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GDModel;

namespace GEDmill.Model
{
    // Indicates preference in cases where multiple records for a one-off event exist (e.g. birth)
    public enum EventPreference
    {
        First,
        Subsequent,
        Unknown
    }

    /// <summary>
    /// Encapsulates an event in an individual's life for sorting into chronological order.
    /// </summary>
    public class Event : IComparable<Event>
    {
        // Date or date range for the event
        private GDMDateValue fDate;

        // If the event is important
        private bool fImportant;

        // String describing the event
        private string fDescription;

        // Note text for the event
        private string fNote;

        // Event overview from GEDCOM _OVER tag
        private string fOverview;

        // GEDCOM tag for event
        private string fType;

        // Indicates that this event is the preferred one, in cases where multiple records for a one-off event exist (e.g. birth)
        private EventPreference fPreference;


        public string Date
        {
            get {
                return (fDate == null) ? null : fDate.GetDisplayString(DateFormat.dfYYYY_MM_DD, true, false);
            }
        }

        public bool Important
        {
            get { return fImportant; }
        }

        public string Note
        {
            get { return fNote; }
        }

        public string Overview
        {
            get { return fOverview; }
        }

        public EventPreference Preference
        {
            get { return fPreference; }
            set { fPreference = value; }
        }

        public string Type
        {
            get { return fType; }
        }


        public Event(GDMDateValue date, string evType, string description, string overview, string note,
            bool important, bool capitalise)
        {
            fPreference = EventPreference.Unknown;
            fDate = date;
            if (capitalise) {
                GMHelper.Capitalise(ref description);
            }
            if (description.Length > 0) {
                if (description[description.Length - 1] == '.') {
                    fDescription = description.Substring(0, description.Length - 1);
                } else {
                    fDescription = description;
                }
            }
            fOverview = overview;
            fImportant = important;
            fNote = note;
            fType = evType;
        }

        // Returns the event description string.
        public override string ToString()
        {
            return fDescription;
        }

        // Used when sorting events into chronological order. If no dates are explicitly attached to the event, tries to work out an order.
        public int CompareTo(Event other)
        {
            if (fDate != null && other.fDate != null) {
                int res = fDate.CompareTo(other.fDate);
                if (res != 0)
                    return res;

                // Some events naturally precede others: BIRTH, MARRIAGE, DEATH, BURIAL
                int precedence = Precedence - other.Precedence;
                if (precedence != 0) {
                    return precedence;
                }

                // Sort alphabetically if all else fails.
                return fDescription.CompareTo(other.fDescription);
            } else if (other.fDate != null) {
                // This goes after valid dates.
                return 1;
            } else if (fDate != null) {
                // This goes before invalid dates.
                return -1;
            }

            // Some events naturally precede others: BIRTH, MARRIAGE, DEATH, BURIAL
            int precedence2 = Precedence - other.Precedence;
            if (precedence2 != 0) {
                return precedence2;
            }

            // Sort alphabetically if all else fails
            return fDescription.CompareTo(other.fDescription);
        }

        // Returns a value between 0 (earliest) and 100 (latest) used to
        // sort events if explicit date isn't present.
        private int Precedence
        {
            get {
                switch (Type) {
                    case "BIRT":
                        return 3;
                    case "CHR":
                        return 6;
                    case "BAPM":
                    case "BASM":
                    case "BLES":
                        return 9;
                    case "CONF":
                    case "BARM":
                        return 12;
                    case "FCOM": // first communion
                        return 15;
                    case "ADOP":
                        return 18;
                    case "CHRA": // christened as adult
                        return 21;
                    case "NATU": //naturalized
                        return 24;
                    case "GRAD":
                        return 27;
                    case "ORDN": //ordained
                        return 30;
                    case "ENGA":
                        return 33;
                    case "MARB":
                    case "MARC":
                    case "MARL":
                        return 36;
                    case "MARR":
                        return 39;
                    case "GEDMILL_ADOPTION_OF_CHILD":
                        return 42;
                    case "NCHI":
                        return 45;
                    case "EVEN":
                    case "EMIG":
                    case "IMMI":
                    case "CAST":
                    case "DSCR":
                    case "EDUC":
                    case "IDNO":
                    case "NATI":
                    case "NMR":
                    case "OCCU":
                    case "PROP":
                    case "RELI":
                    case "RESI":
                    case "SSN":
                    case "TITL":
                    case "FACT":
                    case "_AKA": // _AKA Brother's Keeper
                    case "_AKAN": // _AKAN Brother's Keeper
                    case "CENS":
                        // These could have occured at any time in their life, so return middle value
                        return 50;
                    case "DIVF":
                        return 53;
                    case "DIV":
                    case "ANUL"://annulment of marriage";
                        return 56;
                    case "MARS":
                        return 59;
                    case "_NMR": // _NMR Brother's Keeper never married
                        return 62;
                    case "RETI":
                    case "WILL":
                        return 65;
                    case "PROB":
                        return 68;
                    case "DEAT":
                        return 71;
                    case "BURI":
                        return 73;
                    case "CREM":
                        return 76;

                    default:
                        return 50;
                }
            }
        }
    }
}
