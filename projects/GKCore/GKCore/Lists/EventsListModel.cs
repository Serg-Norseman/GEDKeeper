/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GKCore.Interfaces;
using GKCore.Design.Views;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public sealed class EventsListModel : SheetModel<GDMCustomEvent>
    {
        private readonly bool fPersonsMode;

        public EventsListModel(IBaseWindow baseWin, ChangeTracker undoman, bool personsMode) : base(baseWin, undoman)
        {
            fPersonsMode = personsMode;
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown,
                RecordAction.raCopy, RecordAction.raPaste);

            fListColumns.AddColumn(LSID.LSID_NumberSym, 25, false);
            fListColumns.AddColumn(LSID.LSID_Event, 90, false);
            fListColumns.AddColumn(LSID.LSID_Date, 80, false);
            if (!fPersonsMode) {
                fListColumns.AddColumn(LSID.LSID_Place, 200, false);
            } else {
                fListColumns.AddColumn(LSID.LSID_PlaceAndAttribute, 200, false);
            }
            fListColumns.AddColumn(LSID.LSID_Cause, 130, false);
            fListColumns.ResetDefaults();
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
                    if (fPersonsMode) {
                        string st = fFetchedRec.Place.StringValue;
                        if (fFetchedRec.StringValue != "") {
                            st = st + " [" + fFetchedRec.StringValue + "]";
                        }
                        result = st;
                    } else {
                        result = fFetchedRec.Place.StringValue;
                    }
                    break;
                case 4:
                    result = GKUtils.GetEventCause(fFetchedRec);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as GDMRecordWithEvents;
            if (dataOwner == null) return;

            try {
                UpdateStructList(dataOwner.Events);
            } catch (Exception ex) {
                Logger.WriteError("EventsListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            GDMRecordWithEvents record = fDataOwner as GDMRecordWithEvents;
            if (fBaseWin == null || record == null) return;

            GDMCustomEvent evt = eArgs.ItemData as GDMCustomEvent;

            bool result = false;

            try {
                switch (eArgs.Action) {
                    case RecordAction.raAdd:
                    case RecordAction.raEdit:
                        using (var dlgEventEdit = AppHost.ResolveDialog<IEventEditDlg>(fBaseWin)) {
                            bool exists = (evt != null);

                            GDMCustomEvent newEvent;
                            if (evt != null) {
                                newEvent = evt;
                            } else {
                                if (record is GDMIndividualRecord) {
                                    newEvent = new GDMIndividualEvent();
                                } else {
                                    newEvent = new GDMFamilyEvent();
                                }
                            }

                            dlgEventEdit.Event = newEvent;
                            result = AppHost.Instance.ShowModalX(dlgEventEdit, true);

                            if (!result) {
                                if (!exists) {
                                    newEvent.Dispose();
                                }
                            } else {
                                newEvent = dlgEventEdit.Event;

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
                        if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemoveEventQuery))) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otRecordEventRemove, record, evt);
                            evt = null;
                        }
                        break;

                    case RecordAction.raMoveUp:
                    case RecordAction.raMoveDown:
                        {
                            int idx = record.Events.IndexOf(evt);
                            switch (eArgs.Action) {
                                case RecordAction.raMoveUp:
                                    record.Events.Exchange(idx - 1, idx);
                                    break;

                                case RecordAction.raMoveDown:
                                    record.Events.Exchange(idx, idx + 1);
                                    break;
                            }
                            result = true;
                        }
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
    }
}
