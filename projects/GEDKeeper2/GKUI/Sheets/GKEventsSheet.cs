/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Dialogs;

namespace GKUI.Sheets
{
    public sealed class GKEventsSheet : GKCustomSheet
    {
        private readonly bool fPersonsMode;

        public GKEventsSheet(IBaseEditor baseEditor, Control aOwner, bool personsMode) : base(baseEditor, aOwner)
        {
            this.fPersonsMode = personsMode;

            this.Columns_BeginUpdate();
            this.AddColumn("№", 25, false);
            this.AddColumn(LangMan.LS(LSID.LSID_Event), 90, false);
            this.AddColumn(LangMan.LS(LSID.LSID_Date), 80, false);
            if (!fPersonsMode) {
                this.AddColumn(LangMan.LS(LSID.LSID_Place), 200, false);
            } else {
                this.AddColumn(LangMan.LS(LSID.LSID_PlaceAndAttribute), 200, false);
            }
            this.AddColumn(LangMan.LS(LSID.LSID_Cause), 130, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete,
                                                       SheetButton.lbMoveUp, SheetButton.lbMoveDown);

            this.OnModify += this.ListModify;
        }

        public override void UpdateSheet()
        {
            if (this.DataList == null) return;
            
            try
            {
                this.ClearItems();

                int idx = 0;
                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                    GEDCOMCustomEvent evt = this.DataList.Current as GEDCOMCustomEvent;
                    if (evt == null) continue;

                    idx += 1;
                    
                    if (this.fPersonsMode)
                    {
                        GKListItem item = this.AddItem(idx, evt);
                        item.AddSubItem(GKUtils.GetIndividualEventName(evt));
                        item.AddSubItem(evt.Detail.Date.Value);
                        string st = evt.Detail.Place.StringValue;
                        if (evt.StringValue != "") {
                            st = st + " [" + evt.StringValue + "]";
                        }
                        item.AddSubItem(st);
                        item.AddSubItem(GKUtils.GetEventCause(evt.Detail));
                    }
                    else
                    {
                        GKListItem item = this.AddItem(idx, evt);
                        item.AddSubItem(GKUtils.GetFamilyEventName(evt as GEDCOMFamilyEvent));
                        item.AddSubItem(evt.Detail.Date.Value);
                        item.AddSubItem(evt.Detail.Place.StringValue);
                        item.AddSubItem(GKUtils.GetEventCause(evt.Detail));
                    }
                }

                this.ResizeColumn(1);
                this.ResizeColumn(2);
                this.ResizeColumn(3);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKEventsSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            if (this.DataList == null) return;

            IBaseWindow aBase = this.Editor.Base;
            if (aBase == null) return;

            GEDCOMCustomEvent evt = eArgs.ItemData as GEDCOMCustomEvent;

            bool result = ModifyRecEvent(aBase, this.DataList.Owner as GEDCOMRecordWithEvents, ref evt, eArgs.Action);

            if (result && eArgs.Action == RecordAction.raAdd) eArgs.ItemData = evt;

            if (result) {
                aBase.Modified = true;
                this.UpdateSheet();
            }
        }

        // FIXME
        private static bool ModifyRecEvent(IBaseWindow aBase, GEDCOMRecordWithEvents record, ref GEDCOMCustomEvent aEvent, RecordAction action)
        {
            bool result = false;

            try
            {
                switch (action)
                {
                    case RecordAction.raAdd:
                    case RecordAction.raEdit:
                        using (EventEditDlg fmEventEdit = new EventEditDlg(aBase))
                        {
                            GEDCOMCustomEvent newEvent;
                            if (aEvent != null) {
                                newEvent = aEvent;
                            } else {
                                if (record is GEDCOMIndividualRecord) {
                                    newEvent = new GEDCOMIndividualEvent(aBase.Tree, record, "", "");
                                } else {
                                    newEvent = new GEDCOMFamilyEvent(aBase.Tree, record, "", "");
                                }
                            }

                            fmEventEdit.Event = newEvent;
                            DialogResult dialogResult = MainWin.Instance.ShowModalEx(fmEventEdit, true);

                            if (dialogResult != DialogResult.OK)
                            {
                                if (dialogResult == DialogResult.Cancel && aEvent == null) {
                                    newEvent.Dispose();
                                }
                            }
                            else
                            {
                                newEvent = fmEventEdit.Event;

                                if (aEvent == null) {
                                    record.AddEvent(newEvent);
                                } else {
                                    if (record is GEDCOMIndividualRecord && newEvent != aEvent) {
                                        record.Events.Delete(aEvent);
                                        record.AddEvent(newEvent);
                                    }
                                }

                                aEvent = newEvent;

                                aBase.CollectEventValues(aEvent);

                                result = true;
                            }
                        }
                        break;

                    case RecordAction.raDelete:
                        if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_RemoveEventQuery)) != DialogResult.No) {
                            record.Events.Delete(aEvent);
                            aEvent = null;
                            result = true;
                        }
                        break;

                    case RecordAction.raMoveUp:
                    case RecordAction.raMoveDown:
                        {
                            int idx = record.Events.IndexOf(aEvent);
                            switch (action)
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
                return false;
            }

            return result;
        }
    }
}
