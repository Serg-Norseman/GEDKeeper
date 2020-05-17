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
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public sealed class EventsListModel : ListModel
    {
        private readonly bool fPersonsMode;

        public EventsListModel(IBaseWindow baseWin, ChangeTracker undoman, bool personsMode) : base(baseWin, undoman)
        {
            fPersonsMode = personsMode;
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown);

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

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as GDMRecordWithEvents;
            if (fSheetList == null || dataOwner == null) return;

            try
            {
                fSheetList.ClearItems();

                for (int i = 0; i < dataOwner.Events.Count; i++)
                {
                    GDMCustomEvent evt = dataOwner.Events[i];

                    object[] itemsData = new object[5];
                    itemsData[0] = (i + 1);
                    itemsData[1] = GKUtils.GetEventName(evt);
                    itemsData[2] = new GDMDateItem(evt.Date.Value);
                    if (fPersonsMode) {
                        string st = evt.Place.StringValue;
                        if (evt.StringValue != "") {
                            st = st + " [" + evt.StringValue + "]";
                        }
                        itemsData[3] = st;
                    } else {
                        itemsData[3] = evt.Place.StringValue;
                    }
                    itemsData[4] = GKUtils.GetEventCause(evt);

                    fSheetList.AddItem(evt, itemsData);
                }

                fSheetList.ResizeColumn(1);
                fSheetList.ResizeColumn(2);
                fSheetList.ResizeColumn(3);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("EventsListModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fBaseWin == null || fSheetList == null || dataOwner == null) return;

            GDMCustomEvent evt = eArgs.ItemData as GDMCustomEvent;
            GDMRecordWithEvents record = dataOwner as GDMRecordWithEvents;

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
                                    newEvent = new GDMIndividualEvent(record);
                                } else {
                                    newEvent = new GDMFamilyEvent(record);
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
                }
            } catch (Exception ex) {
                Logger.LogWrite("EventsListModel.Modify(): " + ex.Message);
                result = false;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd) {
                    eArgs.ItemData = evt;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
