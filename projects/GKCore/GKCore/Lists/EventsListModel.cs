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

using System;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Lists
{
    public sealed class EventsListModel : ListModel
    {
        private readonly bool fPersonsMode;

        public EventsListModel(IBaseWindow baseWin, ChangeTracker undoman, bool personsMode) : base(baseWin, undoman)
        {
            fPersonsMode = personsMode;
        }

        public override void InitView()
        {
            fSheetList.AddColumn("№", 25, false);
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Event), 90, false);
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Date), 80, false);
            if (!fPersonsMode) {
                fSheetList.AddColumn(LangMan.LS(LSID.LSID_Place), 200, false);
            } else {
                fSheetList.AddColumn(LangMan.LS(LSID.LSID_PlaceAndAttribute), 200, false);
            }
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Cause), 130, false);

            fSheetList.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete,
                                                             SheetButton.lbMoveUp, SheetButton.lbMoveDown);
        }

        public override void UpdateContent()
        {
            var dataOwner = fDataOwner as GEDCOMRecordWithEvents;
            if (fSheetList == null || dataOwner == null) return;

            try
            {
                fSheetList.ClearItems();

                for (int i = 0; i < dataOwner.Events.Count; i++)
                {
                    GEDCOMCustomEvent evt = dataOwner.Events[i];

                    IListItem item = fSheetList.AddItem(i + 1, evt);
                    item.AddSubItem(GKUtils.GetEventName(evt));
                    item.AddSubItem(new GEDCOMDateItem(evt.Date.Value));
                    if (fPersonsMode) {
                        string st = evt.Place.StringValue;
                        if (evt.StringValue != "") {
                            st = st + " [" + evt.StringValue + "]";
                        }
                        item.AddSubItem(st);
                    } else {
                        item.AddSubItem(evt.Place.StringValue);
                    }
                    item.AddSubItem(GKUtils.GetEventCause(evt));

                    /*GKListSubItem[] itemsData = new GKListSubItem[4];

                    itemsData[0] = new GKListSubItem(GKUtils.GetEventName(evt));
                    itemsData[1] = new GKListSubItem(new GEDCOMDateItem(evt.Date.Value));
                    if (fPersonsMode) {
                        string st = evt.Place.StringValue;
                        if (evt.StringValue != "") {
                            st = st + " [" + evt.StringValue + "]";
                        }
                        itemsData[2] = new GKListSubItem(st);
                    } else {
                        itemsData[2] = new GKListSubItem(evt.Place.StringValue);
                    }
                    itemsData[3] = new GKListSubItem(GKUtils.GetEventCause(evt));

                    fSheetList.AddItem((i + 1), evt, itemsData);*/
                }

                fSheetList.ResizeColumn(1);
                fSheetList.ResizeColumn(2);
                fSheetList.ResizeColumn(3);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKEventsSheet.UpdateSheet(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fBaseWin == null || fSheetList == null || dataOwner == null) return;

            GEDCOMCustomEvent evt = eArgs.ItemData as GEDCOMCustomEvent;
            GEDCOMRecordWithEvents record = dataOwner as GEDCOMRecordWithEvents;

            bool result = false;

            try
            {
                switch (eArgs.Action)
                {
                    case RecordAction.raAdd:
                    case RecordAction.raEdit:
                        using (var dlgEventEdit = AppHost.Container.Resolve<IEventEditDlg>())
                        {
                            dlgEventEdit.InitDialog(fBaseWin);

                            bool exists = (evt != null);

                            GEDCOMCustomEvent newEvent;
                            if (evt != null) {
                                newEvent = evt;
                            } else {
                                if (record is GEDCOMIndividualRecord) {
                                    newEvent = new GEDCOMIndividualEvent(fBaseWin.Context.Tree, record, "", "");
                                } else {
                                    newEvent = new GEDCOMFamilyEvent(fBaseWin.Context.Tree, record, "", "");
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
                                    if (record is GEDCOMIndividualRecord && newEvent != evt) {
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
                        if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemoveEventQuery)) != false) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otRecordEventRemove, record, evt);
                            evt = null;
                        }
                        break;

                    case RecordAction.raMoveUp:
                    case RecordAction.raMoveDown:
                        {
                            int idx = record.Events.IndexOf(evt);
                            switch (eArgs.Action)
                            {
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
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKEventsSheet.ModifyRecEvent(): " + ex.Message);
                result = false;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd) {
                    eArgs.ItemData = evt;
                }

                fBaseWin.Modified = true;
                fSheetList.UpdateSheet();
            }
        }
    }
}
