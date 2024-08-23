/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;

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

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stEvents);

            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.Event, 90, false);
            result.AddColumn(LSID.Date, 90, false);
            result.AddColumn(LSID.PlaceAndAttribute, 200, false);
            result.AddColumn(LSID.Cause, 130, false);
            result.AddColumn(LSID.RPSources, 32, false);
            result.AddColumn(LSID.RPNotes, 32, false);
            result.AddColumn(LSID.RPMultimedia, 32, false);

            result.ResetDefaults();
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
                    result = GKUtils.GetEventPlaceAndAttributeValues(fFetchedRec);
                    break;
                case 4:
                    result = GKUtils.GetEventCause(fFetchedRec);
                    break;
                case 5:
                    result = fFetchedRec.HasSourceCitations ? (fOptions.ShowNumberOfSubstructures ? fFetchedRec.SourceCitations.Count.ToString() : GKData.CHECK_MARK) : string.Empty;
                    break;
                case 6:
                    result = fFetchedRec.HasNotes ? (fOptions.ShowNumberOfSubstructures ? fFetchedRec.Notes.Count.ToString() : GKData.CHECK_MARK) : string.Empty;
                    break;
                case 7:
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
                        result = record.Events.Exchange(evt, eArgs.Action);
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
