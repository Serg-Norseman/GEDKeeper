/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class EventEditDlgController : DialogController<IEventEditDlg>
    {
        private GDMCustomEvent fEvent;
        private GDMLocationRecord fTempLocation;


        public GDMCustomEvent Event
        {
            get { return fEvent; }
            set {
                if (fEvent != value) {
                    fEvent = value;
                    UpdateView();
                }
            }
        }


        public EventEditDlgController(IEventEditDlg view) : base(view)
        {
            fTempLocation = null;

            int num = GKData.DateKinds.Length;
            for (int i = 0; i < num; i++) {
                fView.EventDateType.Add(LangMan.LS(GKData.DateKinds[i].Name));
            }

            for (GDMCalendar gc = GDMCalendar.dcGregorian; gc <= GDMCalendar.dcLast; gc++) {
                GKData.CalendarStruct cdr = GKData.DateCalendars[(int)gc];
                if (!cdr.HasSupport) continue;

                fView.Date1Calendar.AddItem(LangMan.LS(cdr.Name), gc);
                fView.Date2Calendar.AddItem(LangMan.LS(cdr.Name), gc);
            }

            fView.Date1Calendar.SelectedIndex = 0;
            fView.Date2Calendar.SelectedIndex = 0;

            fView.EventType.Activate();
        }

        private GDMCustomDate AssembleDate()
        {
            GDMCustomDate result = null;

            GDMCalendar cal1 = fView.Date1Calendar.GetSelectedTag<GDMCalendar>();
            GDMCalendar cal2 = fView.Date2Calendar.GetSelectedTag<GDMCalendar>();

            GDMDate gcd1 = GDMDate.CreateByFormattedStr(fView.Date1.NormalizeDate, cal1, true);
            if (gcd1 == null) throw new ArgumentNullException("gcd1");

            GDMDate gcd2 = GDMDate.CreateByFormattedStr(fView.Date2.NormalizeDate, cal2, true);
            if (gcd2 == null) throw new ArgumentNullException("gcd2");

            gcd1.YearBC = fView.Date1BC.Checked;
            gcd2.YearBC = fView.Date2BC.Checked;

            switch (fView.EventDateType.SelectedIndex) {
                case 0:
                    result = gcd1;
                    break;

                case 1: // BEF gcd2
                    result = GDMCustomDate.CreateRange(null, null, gcd2);
                    break;

                case 2: // AFT gcd1
                    result = GDMCustomDate.CreateRange(null, gcd1, null);
                    break;

                case 3: // "BET " + gcd1 + " AND " + gcd2
                    result = GDMCustomDate.CreateRange(null, gcd1, gcd2);
                    break;

                case 4: // FROM gcd1
                    result = GDMCustomDate.CreatePeriod(null, gcd1, null);
                    break;

                case 5: // TO gcd2
                    result = GDMCustomDate.CreatePeriod(null, null, gcd2);
                    break;

                case 6: // FROM gcd1 TO gcd2
                    result = GDMCustomDate.CreatePeriod(null, gcd1, gcd2);
                    break;

                case 7: // ABT gcd1
                    result = GDMCustomDate.CreateApproximated(null, gcd1, GDMApproximated.daAbout);
                    break;

                case 8: // CAL gcd1
                    result = GDMCustomDate.CreateApproximated(null, gcd1, GDMApproximated.daCalculated);
                    break;

                case 9: // EST gcd1
                    result = GDMCustomDate.CreateApproximated(null, gcd1, GDMApproximated.daEstimated);
                    break;
            }

            return result;
        }

        public override bool Accept()
        {
            try {
                fEvent.Place.StringValue = fView.Place.Text;
                fBase.Context.Tree.SetPtrValue(fEvent.Place.Location, fTempLocation);
                fEvent.Classification = fView.EventName.Text;
                fEvent.Cause = fView.Cause.Text;
                fEvent.Agency = fView.Agency.Text;

                GDMCustomDate dt = AssembleDate();
                if (dt == null) throw new ArgumentNullException("dt");

                fEvent.Date.ParseString(dt.StringValue);

                int eventType = fView.EventType.SelectedIndex;
                if (fEvent is GDMFamilyEvent) {
                    fEvent.SetName(GKData.FamilyEvents[eventType].Sign);
                } else {
                    GKData.EventStruct eventProps = GKData.PersonEvents[eventType];
                    fEvent.SetName(eventProps.Sign);
                    fEvent.StringValue = (eventProps.Kind == PersonEventKind.ekFact) ? fView.Attribute.Text : string.Empty;
                }

                if (fEvent is GDMIndividualEvent) {
                    if (GKData.PersonEvents[eventType].Kind == PersonEventKind.ekFact) {
                        var attr = new GDMIndividualAttribute(null);
                        attr.Assign(fEvent);
                        fEvent = attr;
                    }
                }

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("EventEditController.Accept()", ex);
                return false;
            }
        }

        private void SetEventTypes(GKData.EventStruct[] eventTypes)
        {
            fView.EventType.Clear();
            int num = eventTypes.Length;
            for (int i = 0; i < num; i++) {
                fView.EventType.Add(LangMan.LS(eventTypes[i].Name));
            }
        }

        public override void UpdateView()
        {
            fView.NotesList.ListModel.DataOwner = fEvent;
            fView.MediaList.ListModel.DataOwner = fEvent;
            fView.SourcesList.ListModel.DataOwner = fEvent;

            var evtName = fEvent.GetTagName();
            if (fEvent is GDMFamilyEvent) {
                SetEventTypes(GKData.FamilyEvents);
                int idx = GKUtils.GetFamilyEventIndex(evtName);
                if (idx < 0) idx = 0;
                fView.EventType.SelectedIndex = idx;
            } else {
                SetEventTypes(GKData.PersonEvents);
                int idx = GKUtils.GetPersonEventIndex(evtName);
                if (idx < 0) idx = 0;
                fView.EventType.SelectedIndex = idx;

                if (idx >= 0 && GKData.PersonEvents[idx].Kind == PersonEventKind.ekFact) {
                    fView.Attribute.Text = fEvent.StringValue;
                }
            }

            ChangeEventType();

            GDMCustomDate date = fEvent.Date.Value;

            if (date is GDMDateRange) {
                GDMDateRange dtRange = date as GDMDateRange;

                if (dtRange.After.StringValue == "" && dtRange.Before.StringValue != "") {
                    fView.EventDateType.SelectedIndex = 1;
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue == "") {
                    fView.EventDateType.SelectedIndex = 2;
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue != "") {
                    fView.EventDateType.SelectedIndex = 3;
                }

                fView.Date1.NormalizeDate = dtRange.After.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date2.NormalizeDate = dtRange.Before.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date1Calendar.SetSelectedTag<GDMCalendar>(dtRange.After.DateCalendar);
                fView.Date2Calendar.SetSelectedTag<GDMCalendar>(dtRange.Before.DateCalendar);
                fView.Date1BC.Checked = dtRange.After.YearBC;
                fView.Date2BC.Checked = dtRange.Before.YearBC;
            } else if (date is GDMDatePeriod) {
                GDMDatePeriod dtPeriod = date as GDMDatePeriod;

                if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue == "") {
                    fView.EventDateType.SelectedIndex = 4;
                } else if (dtPeriod.DateFrom.StringValue == "" && dtPeriod.DateTo.StringValue != "") {
                    fView.EventDateType.SelectedIndex = 5;
                } else if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue != "") {
                    fView.EventDateType.SelectedIndex = 6;
                }

                fView.Date1.NormalizeDate = dtPeriod.DateFrom.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date2.NormalizeDate = dtPeriod.DateTo.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date1Calendar.SetSelectedTag<GDMCalendar>(dtPeriod.DateFrom.DateCalendar);
                fView.Date2Calendar.SetSelectedTag<GDMCalendar>(dtPeriod.DateTo.DateCalendar);
                fView.Date1BC.Checked = dtPeriod.DateFrom.YearBC;
                fView.Date2BC.Checked = dtPeriod.DateTo.YearBC;
            } else if (date is GDMDate) {
                GDMApproximated approximated = (date as GDMDate).Approximated;

                switch (approximated) {
                    case GDMApproximated.daExact:
                        fView.EventDateType.SelectedIndex = 0;
                        break;
                    case GDMApproximated.daAbout:
                        fView.EventDateType.SelectedIndex = 7;
                        break;
                    case GDMApproximated.daCalculated:
                        fView.EventDateType.SelectedIndex = 8;
                        break;
                    case GDMApproximated.daEstimated:
                        fView.EventDateType.SelectedIndex = 9;
                        break;
                }

                fView.Date1.NormalizeDate = (date as GDMDate).GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Date1Calendar.SetSelectedTag<GDMCalendar>((date as GDMDate).DateCalendar);
                fView.Date1BC.Checked = (date as GDMDate).YearBC;
            } else {
                fView.EventDateType.SelectedIndex = 0;
                fView.Date1.NormalizeDate = "";
                fView.Date1Calendar.SetSelectedTag<GDMCalendar>(GDMCalendar.dcGregorian);
                fView.Date1BC.Checked = false;
            }

            ChangeDateType();
            fView.EventName.Text = fEvent.Classification;
            fView.Cause.Text = fEvent.Cause;
            fView.Agency.Text = fEvent.Agency;

            fTempLocation = fBase.Context.Tree.GetPtrValue<GDMLocationRecord>(fEvent.Place.Location);
            UpdatePlace();

            fView.NotesList.UpdateSheet();
            fView.MediaList.UpdateSheet();
            fView.SourcesList.UpdateSheet();
        }

        private void UpdatePlace()
        {
            if (fTempLocation != null) {
                fView.Place.Text = fTempLocation.LocationName;
                fView.SetLocationMode(true);
            } else {
                fView.Place.Text = fEvent.Place.StringValue;
                fView.SetLocationMode(false);
            }
        }

        public void AddPlace()
        {
            fTempLocation = fBase.Context.SelectRecord(GDMRecordType.rtLocation, null) as GDMLocationRecord;
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

        public void ChangeDateType()
        {
            int idx = fView.EventDateType.SelectedIndex;
            if (idx < 0 || idx >= GKData.DateKinds.Length) return;

            byte dates = GKData.DateKinds[idx].Dates;
            bool vis1 = BitHelper.IsSetBit(dates, 0);
            bool vis2 = BitHelper.IsSetBit(dates, 1);

            fView.Date1.Enabled = vis1;
            fView.Date1Calendar.Enabled = vis1;
            fView.Date1BC.Enabled = vis1;

            fView.Date2.Enabled = vis2;
            fView.Date2Calendar.Enabled = vis2;
            fView.Date2BC.Enabled = vis2;
        }

        private void SetAttributeMode(bool active)
        {
            if (active) {
                fView.Attribute.Enabled = true;
                //txtAttribute.BackColor = SystemColors.Window;
            } else {
                fView.Attribute.Enabled = false;
                //txtAttribute.BackColor = SystemColors.Control;
                fView.Attribute.Text = "";
            }
        }

        public void ChangeEventType()
        {
            if (fEvent is GDMFamilyEvent) {
                SetAttributeMode(false);
            } else {
                int idx = fView.EventType.SelectedIndex;
                if (idx >= 0) {
                    if (GKData.PersonEvents[idx].Kind == PersonEventKind.ekEvent) {
                        SetAttributeMode(false);
                    } else {
                        SetAttributeMode(true);
                    }
                }
            }

            string evName;
            int id = fView.EventType.SelectedIndex;
            if (fEvent is GDMFamilyEvent) {
                evName = GKData.FamilyEvents[id].Sign;
            } else {
                evName = GKData.PersonEvents[id].Sign;
            }

            // TODO: It is necessary to provide the registrable list of values for different tag types.
            string[] vals;
            bool canbeSorted, userInput;

            if (evName == GEDCOMTagName._BGRO) {
                vals = GKData.BloodGroups.Split('|');
                canbeSorted = false;
                userInput = false;
            } else {
                vals = fBase.Context.ValuesCollection.GetValues(evName);
                canbeSorted = true;
                userInput = true;
            }

            if (vals != null) {
                string tmp = fView.Attribute.Text;
                fView.Attribute.Clear();
                fView.Attribute.AddRange((object[])vals, canbeSorted);
                fView.Attribute.Text = tmp;
                fView.Attribute.ReadOnly = (!userInput);
            }
        }
    }
}
