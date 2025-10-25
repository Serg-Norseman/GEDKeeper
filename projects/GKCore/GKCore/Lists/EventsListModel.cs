/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;

namespace GKCore.Lists
{
    public sealed class EventsListModel : SheetModel<GDMCustomEvent>
    {
        private readonly GlobalOptions fOptions;

        public EventsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown,
                RecordAction.raCopy, RecordAction.raPaste);

            fOptions = GlobalOptions.Instance;
        }

        protected override void OnDataOwner()
        {
            if (fDataOwner is GDMIndividualRecord) {
                AddCustomAction(LangMan.LS(LSID.CreateCalculatedBirthEvent), CreateCalculatedBirthEvent);
                //AddCustomAction("CreateEstimatedBirthEvent", CreateEstimatedBirthEvent);
                //AddCustomAction("CreateEstimatedDeathEvent", CreateEstimatedDeathEvent);
            }
            base.OnDataOwner();
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stEvents);
            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.Event, 90, false);
            result.AddColumn(LSID.Date, DataType.dtGEDCOMDate, 90, false);
            result.AddColumn(LSID.Age, 80, false);
            result.AddColumn(LSID.PlaceAndAttribute, 200, false);
            result.AddColumn(LSID.Cause, 130, false);
            result.AddColumn(LSID.RPSources, 32, false);
            result.AddColumn(LSID.RPNotes, 32, false);
            result.AddColumn(LSID.RPMultimedia, 32, false);
            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = GKUtils.GetEventName(fFetchedRec);
                    break;
                case 2:
                    result = new GDMDateItem(fFetchedRec.Date.Value);
                    break;
                case 3:
                    result = GKUtils.GetAgeDisplayStr(fFetchedRec);
                    break;
                case 4:
                    result = GKUtils.GetEventPlaceAndAttributeValues(fFetchedRec);
                    break;
                case 5:
                    result = GKUtils.GetEventCause(fFetchedRec);
                    break;
                case 6:
                    result = fFetchedRec.HasSourceCitations ? (fOptions.ShowNumberOfSubstructures ? fFetchedRec.SourceCitations.Count.ToString() : GKData.CHECK_MARK) : string.Empty;
                    break;
                case 7:
                    result = fFetchedRec.HasNotes ? (fOptions.ShowNumberOfSubstructures ? fFetchedRec.Notes.Count.ToString() : GKData.CHECK_MARK) : string.Empty;
                    break;
                case 8:
                    result = fFetchedRec.HasMultimediaLinks ? (fOptions.ShowNumberOfSubstructures ? fFetchedRec.MultimediaLinks.Count.ToString() : GKData.CHECK_MARK) : string.Empty;
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as GDMRecordWithEvents;
            if (dataOwner != null)
                UpdateStructList(dataOwner.Events);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            GDMRecordWithEvents record = fDataOwner as GDMRecordWithEvents;
            if (fBaseWin == null || record == null) return;

            GDMCustomEvent evt = eArgs.ItemData as GDMCustomEvent;

            bool result = false;

            try {
                switch (eArgs.Action) {
                    case RecordAction.raAdd:
                    case RecordAction.raEdit: {
                            bool exists = (evt != null);

                            GDMCustomEvent newEvent;
                            if (exists) {
                                newEvent = evt;
                            } else {
                                if (record is GDMIndividualRecord) {
                                    newEvent = new GDMIndividualEvent();
                                } else {
                                    newEvent = new GDMFamilyEvent();
                                }
                            }

                            using (var dlg = AppHost.ResolveDialog<IEventEditDlg>(fBaseWin)) {
                                dlg.Event = newEvent;
                                result = await AppHost.Instance.ShowModalAsync(dlg, fOwner, true);
                                newEvent = dlg.Event; // In this dialog the event object can be replaced
                            }

                            if (!result) {
                                if (!exists) {
                                    newEvent.Dispose();
                                }
                            } else {
                                if (!exists) {
                                    result = fUndoman.DoOrdinaryOperation(OperationType.otRecordEventAdd, record, newEvent);
                                } else {
                                    if (record is GDMIndividualRecord && newEvent != evt) {
                                        fUndoman.DoOrdinaryOperation(OperationType.otRecordEventRemove, record, evt);
                                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordEventAdd, record, newEvent);
                                    }
                                }

                                evt = newEvent;
                                fBaseWin.Context.CollectEventValues(evt);
                            }
                        }
                        break;

                    case RecordAction.raDelete:
                        if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveEventQuery))) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otRecordEventRemove, record, evt);
                            evt = null;
                        }
                        break;

                    case RecordAction.raMoveUp:
                    case RecordAction.raMoveDown:
                        result = Exchange(record.Events, evt, eArgs.Action);
                        break;

                    case RecordAction.raCopy:
                        AppHost.Instance.SetClipboardObj<GDMCustomEvent>(evt);
                        break;

                    case RecordAction.raCut:
                        break;

                    case RecordAction.raPaste:
                        evt = AppHost.Instance.GetClipboardObj<GDMCustomEvent>();
                        if (evt != null) {
                            if (evt is GDMIndividualEvent) {
                                evt = (evt as GDMIndividualEvent).Clone();
                            } else if (evt is GDMIndividualAttribute) {
                                evt = (evt as GDMIndividualAttribute).Clone();
                            } else if (evt is GDMFamilyEvent) {
                                evt = (evt as GDMFamilyEvent).Clone();
                            }
                            result = fUndoman.DoOrdinaryOperation(OperationType.otRecordEventAdd, record, evt);
                        }
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("EventsListModel.Modify()", ex);
                result = false;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raPaste) {
                    eArgs.ItemData = evt;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }

        private void CreateCalculatedBirthEvent(object sender, EventArgs e)
        {
            object itemData = fSheetList.ListView.GetSelectedData();
            if (itemData is GDMIndividualEventDetail evt && evt.Date.Value is GDMDate date) {
                if (!evt.HasAge) {
                    AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.NoAgeOnEventDate));
                    return;
                }

                var newDate = GDMDate.Subtract(date, evt.Age);

                if (!newDate.IsEmpty()) {
                    var newEvent = GKUtils.CreateIndividualEvent(fDataOwner as IGDMRecordWithEvents, newDate, GEDCOMTagName.BIRT, true);
                    newEvent.AssignDerivative(evt);

                    fSheetList.ListView.UpdateContents();
                }
            }
        }

        private static void ExtractEvents(IGDMRecordWithEvents record, GEDCOMTagType eventType, IList<GDMCustomDate> list, bool first, bool parent)
        {
            if (!record.HasEvents) return;

            var subList = new List<GDMCustomDate>();

            int evtType = (int)eventType;
            int num = record.Events.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent evt = record.Events[i];
                if (((!parent && evt.Id == evtType) || (parent && evt.Id != evtType)) && evt.GetChronologicalYear() != 0) {
                    subList.Add(evt.Date.Value);
                }
            }

            if (subList.Count > 0) {
                subList.Sort();

                // From the children's records we extract only one (first or last) births,
                // from the parent's record - the first and last significant event
                if (parent) {
                    list.Add(subList[0]);
                    list.Add(subList[subList.Count - 1]);
                } else {
                    var dtx = first ? subList[0] : subList[subList.Count - 1];
                    list.Add(dtx);
                }
            }
        }

        private static IList<GDMCustomDate> GetIndiEvents(GDMTree tree, GDMIndividualRecord indiRec, GEDCOMTagType targetEventType)
        {
            var children = new List<GDMIndividualRecord>();
            for (int j = 0, jNum = indiRec.SpouseToFamilyLinks.Count; j < jNum; j++) {
                GDMFamilyRecord famRec = tree.GetPtrValue(indiRec.SpouseToFamilyLinks[j]);
                for (int i = 0, iNum = famRec.Children.Count; i < iNum; i++) {
                    GDMIndividualRecord child = tree.GetPtrValue(famRec.Children[i]);
                    children.Add(child);
                }
            }

            var events = new List<GDMCustomDate>();

            for (int i = 0, iNum = children.Count; i < iNum; i++) {
                var child = children[i];
                bool last = (i != 0 && i == iNum - 1);
                ExtractEvents(child, GEDCOMTagType.BIRT, events, !last, false);
            }

            ExtractEvents(indiRec, targetEventType, events, false, true);

            events.Sort();

            return events;
        }

        private void CreateEstimatedBirthEvent(object sender, EventArgs e)
        {
            if (fDataOwner is GDMIndividualRecord indiRec) {
                var events = GetIndiEvents(fBaseContext.Tree, indiRec, GEDCOMTagType.BIRT);
                if (events.Count < 1) return;

                var firstEvt = events[0];

                var age = new GDMAge();
                // If the source of the calculation is the child's date of birth, then that's correct,
                // but what if the source is the date of another event in life?
                //age.ParseString($"{GKData.MIN_PARENT_AGE}y");
                age.ParseString("5y");
                GDMCustomDate newDate = GDMDate.Subtract(firstEvt as GDMDate, age);

                if (newDate != null) {
                    newDate = GDMCustomDate.CreateRange(null, newDate as GDMDate); // before
                    var newEvent = GKUtils.CreateIndividualEvent(indiRec, newDate, GEDCOMTagName.BIRT);
                    newEvent.Agency = "Estimated using some other event dates";
                    fSheetList.ListView.UpdateContents();
                }
            }
        }

        private void CreateEstimatedDeathEvent(object sender, EventArgs e)
        {
            if (fDataOwner is GDMIndividualRecord indiRec) {
                var events = GetIndiEvents(fBaseContext.Tree, indiRec, GEDCOMTagType.DEAT);
                if (events.Count < 1) return;

                var lastEvt = events[events.Count - 1];

                GDMCustomDate newDate = null;
                if (indiRec.Sex == GDMSex.svMale) {
                    var age = new GDMAge();
                    age.ParseString("9m");
                    newDate = GDMDate.Subtract(lastEvt as GDMDate, age);
                } else if (indiRec.Sex == GDMSex.svFemale) {
                    newDate = lastEvt;
                }

                if (newDate != null) {
                    newDate = GDMCustomDate.CreateRange(newDate as GDMDate, null); // after
                    var newEvent = GKUtils.CreateIndividualEvent(indiRec, newDate, GEDCOMTagName.DEAT);
                    newEvent.Agency = "Estimated using some other event dates";
                    fSheetList.ListView.UpdateContents();
                }
            }
        }
    }
}
