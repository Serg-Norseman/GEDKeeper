/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class EventEditController : DialogController
    {
        private readonly IEventEditDlg fView;

        private GEDCOMCustomEvent fEvent;
        private GEDCOMLocationRecord fTempLocation;


        public GEDCOMCustomEvent Event
        {
            get { return fEvent; }
            set {
                if (fEvent != value) {
                    fEvent = value;
                    UpdateView();
                }
            }
        }


        public EventEditController(IEventEditDlg view)
        {
            fView = view;
            fTempLocation = null;
        }

        private GEDCOMCustomDate AssembleDate()
        {
            GEDCOMCustomDate result = null;

            GEDCOMCalendar cal1 = fView.Date1Calendar;
            GEDCOMCalendar cal2 = fView.Date2Calendar;

            GEDCOMDate gcd1 = GEDCOMDate.CreateByFormattedStr(fView.Date1Text, cal1, true);
            if (gcd1 == null) throw new ArgumentNullException("gcd1");

            GEDCOMDate gcd2 = GEDCOMDate.CreateByFormattedStr(fView.Date2Text, cal2, true);
            if (gcd2 == null) throw new ArgumentNullException("gcd2");

            gcd1.YearBC = fView.Date1BC;
            gcd2.YearBC = fView.Date2BC;

            switch (fView.EventDateType) {
                case 0:
                    result = gcd1;
                    break;

                case 1: // BEF gcd2
                    result = GEDCOMCustomDate.CreateRange(null, null, null, gcd2);
                    break;

                case 2: // AFT gcd1
                    result = GEDCOMCustomDate.CreateRange(null, null, gcd1, null);
                    break;

                case 3: // "BET " + gcd1 + " AND " + gcd2
                    result = GEDCOMCustomDate.CreateRange(null, null, gcd1, gcd2);
                    break;

                case 4: // FROM gcd1
                    result = GEDCOMCustomDate.CreatePeriod(null, null, gcd1, null);
                    break;

                case 5: // TO gcd2
                    result = GEDCOMCustomDate.CreatePeriod(null, null, null, gcd2);
                    break;

                case 6: // FROM gcd1 TO gcd2
                    result = GEDCOMCustomDate.CreatePeriod(null, null, gcd1, gcd2);
                    break;

                case 7: // ABT gcd1
                    result = GEDCOMCustomDate.CreateApproximated(null, null, gcd1, GEDCOMApproximated.daAbout);
                    break;

                case 8: // CAL gcd1
                    result = GEDCOMCustomDate.CreateApproximated(null, null, gcd1, GEDCOMApproximated.daCalculated);
                    break;

                case 9: // EST gcd1
                    result = GEDCOMCustomDate.CreateApproximated(null, null, gcd1, GEDCOMApproximated.daEstimated);
                    break;
            }

            return result;
        }

        public override bool Accept()
        {
            try {
                fEvent.Place.StringValue = fView.PlaceText;
                fEvent.Place.Location.Value = fTempLocation;
                fEvent.Classification = fView.EventNameText;
                fEvent.Cause = fView.CauseText;
                fEvent.Agency = fView.AgencyText;

                GEDCOMCustomDate dt = AssembleDate();
                if (dt == null) throw new ArgumentNullException("dt");

                fEvent.Date.ParseString(dt.StringValue);

                int eventType = fView.EventType;
                if (fEvent is GEDCOMFamilyEvent) {
                    fEvent.SetName(GKData.FamilyEvents[eventType].Sign);
                } else {
                    fEvent.SetName(GKData.PersonEvents[eventType].Sign);
                    if (GKData.PersonEvents[eventType].Kind == PersonEventKind.ekFact) {
                        fEvent.StringValue = fView.AttributeText;
                    } else {
                        fEvent.StringValue = "";
                    }
                }

                if (fEvent is GEDCOMIndividualEvent) {
                    if (GKData.PersonEvents[eventType].Kind == PersonEventKind.ekFact) {
                        GEDCOMIndividualAttribute attr = new GEDCOMIndividualAttribute(fEvent.Owner, fEvent.Parent, "", "");
                        attr.Assign(fEvent);
                        fEvent = attr;
                    }
                }

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("EventEditController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fEvent is GEDCOMFamilyEvent) {
                fView.SetEventTypes(GKData.FamilyEvents);
                int idx = GKUtils.GetFamilyEventIndex(fEvent.Name);
                if (idx < 0) idx = 0;
                fView.EventType = idx;
            } else {
                fView.SetEventTypes(GKData.PersonEvents);
                int idx = GKUtils.GetPersonEventIndex(fEvent.Name);
                if (idx < 0) idx = 0;
                fView.EventType = idx;

                if (idx >= 0 && GKData.PersonEvents[idx].Kind == PersonEventKind.ekFact) {
                    fView.AttributeText = fEvent.StringValue;
                }
            }

            fView.ChangeEventType();

            GEDCOMCustomDate date = fEvent.Date.Value;

            if (date is GEDCOMDateRange) {
                GEDCOMDateRange dtRange = date as GEDCOMDateRange;

                if (dtRange.After.StringValue == "" && dtRange.Before.StringValue != "") {
                    fView.EventDateType = 1;
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue == "") {
                    fView.EventDateType = 2;
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue != "") {
                    fView.EventDateType = 3;
                }

                fView.Date1Text = dtRange.After.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date2Text = dtRange.Before.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date1Calendar = dtRange.After.DateCalendar;
                fView.Date2Calendar = dtRange.Before.DateCalendar;
                fView.Date1BC = dtRange.After.YearBC;
                fView.Date2BC = dtRange.Before.YearBC;
            } else if (date is GEDCOMDatePeriod) {
                GEDCOMDatePeriod dtPeriod = date as GEDCOMDatePeriod;

                if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue == "") {
                    fView.EventDateType = 4;
                } else if (dtPeriod.DateFrom.StringValue == "" && dtPeriod.DateTo.StringValue != "") {
                    fView.EventDateType = 5;
                } else if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue != "") {
                    fView.EventDateType = 6;
                }

                fView.Date1Text = dtPeriod.DateFrom.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date2Text = dtPeriod.DateTo.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date1Calendar = dtPeriod.DateFrom.DateCalendar;
                fView.Date2Calendar = dtPeriod.DateTo.DateCalendar;
                fView.Date1BC = dtPeriod.DateFrom.YearBC;
                fView.Date2BC = dtPeriod.DateTo.YearBC;
            } else if (date is GEDCOMDate) {
                GEDCOMApproximated approximated = (date as GEDCOMDate).Approximated;

                switch (approximated) {
                    case GEDCOMApproximated.daExact:
                        fView.EventDateType = 0;
                        break;
                    case GEDCOMApproximated.daAbout:
                        fView.EventDateType = 7;
                        break;
                    case GEDCOMApproximated.daCalculated:
                        fView.EventDateType = 8;
                        break;
                    case GEDCOMApproximated.daEstimated:
                        fView.EventDateType = 9;
                        break;
                }

                fView.Date1Text = (date as GEDCOMDate).GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date1Calendar = (date as GEDCOMDate).DateCalendar;
                fView.Date1BC = (date as GEDCOMDate).YearBC;
            } else {
                fView.EventDateType = 0;
                fView.Date1Text = "";
                fView.Date1Calendar = GEDCOMCalendar.dcGregorian;
                fView.Date1BC = false;
            }

            fView.ChangeDateType();
            fView.EventNameText = fEvent.Classification;
            fView.CauseText = fEvent.Cause;
            fView.AgencyText = fEvent.Agency;

            fTempLocation = (fEvent.Place.Location.Value as GEDCOMLocationRecord);
            UpdatePlace();

            fView.NotesList.UpdateSheet();
            fView.MediaList.UpdateSheet();
            fView.SourcesList.UpdateSheet();
        }

        private void UpdatePlace()
        {
            if (fTempLocation != null) {
                fView.PlaceText = fTempLocation.LocationName;
                fView.SetLocationMode(true);
            } else {
                fView.PlaceText = fEvent.Place.StringValue;
                fView.SetLocationMode(false);
            }
        }

        public void AddPlace()
        {
            fTempLocation = (fBase.Context.SelectRecord(GEDCOMRecordType.rtLocation, null) as GEDCOMLocationRecord);
            UpdatePlace();
        }

        public void RemovePlace()
        {
            fTempLocation = null;
            UpdatePlace();
        }

        public void ModifyAddress()
        {
            BaseController.ModifyAddress(fBase, fEvent.Address);
        }
    }
}
