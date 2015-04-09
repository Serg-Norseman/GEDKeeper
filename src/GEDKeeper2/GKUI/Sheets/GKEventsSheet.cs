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

            this.Buttons = EnumSet<GKSheetList.SheetButton>.Create(
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
				GKSheetList.SheetButton.lbDelete, 
				GKSheetList.SheetButton.lbMoveUp, 
				GKSheetList.SheetButton.lbMoveDown
			);

            this.OnModify += this.ListModify;
        }

        public override void UpdateSheet()
        {
        	if (this.DataList == null) return;
        	
            try
            {
                this.List.Items.Clear();

                int idx = 0;
                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                    GEDCOMCustomEvent evt = this.DataList.Current as GEDCOMCustomEvent;
                    idx += 1;
                	
                    if (this.fPersonsMode)
                    {
                        string st = GKUtils.GetIndividualEventName(evt);

                        GKListItem item = this.List.AddItem(idx.ToString(), evt);
                        item.SubItems.Add(st);
                        item.SubItems.Add(GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, TfmGEDKeeper.Instance.Options.DefDateFormat, false));
                        st = evt.Detail.Place.StringValue;
                        if (evt.StringValue != "")
                        {
                            st = st + " [" + evt.StringValue + "]";
                        }
                        item.SubItems.Add(st);
                        item.SubItems.Add(GKUtils.GetEventCause(evt.Detail));
                    }
                    else
                    {
                        string st = GKUtils.GetFamilyEventName(evt as GEDCOMFamilyEvent);

                        GKListItem item = this.List.AddItem(idx.ToString(), evt);
                        item.SubItems.Add(st);
                        item.SubItems.Add(GKUtils.GEDCOMCustomDateToStr(evt.Detail.Date, TfmGEDKeeper.Instance.Options.DefDateFormat, false));
                        item.SubItems.Add(evt.Detail.Place.StringValue);
                        item.SubItems.Add(GKUtils.GetEventCause(evt.Detail));
                    }
                }

                this.List.ResizeColumn(1);
                this.List.ResizeColumn(2);
                this.List.ResizeColumn(3);
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKEventsSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            GEDCOMCustomEvent evt = eArgs.ItemData as GEDCOMCustomEvent;

            bool result = this.ModifyRecEvent(aBase, this.DataList.Owner as GEDCOMRecord, ref evt, eArgs.Action);
            if (result && eArgs.Action == RecordAction.raAdd) eArgs.ItemData = evt;

            if (result) this.UpdateSheet();
        }

        // FIXME
        private bool ModifyRecEvent(IBase aBase, GEDCOMRecord record, ref GEDCOMCustomEvent aEvent, RecordAction action)
        {
            bool result = false;

            try
            {
                switch (action)
                {
                    case RecordAction.raAdd:
                    case RecordAction.raEdit:
                        using (TfmEventEdit fmEventEdit = new TfmEventEdit(aBase))
                        {
                            GEDCOMCustomEvent newEvent;
                            if (aEvent != null)
                            {
                                newEvent = aEvent;
                            }
                            else
                            {
                                if (record is GEDCOMIndividualRecord)
                                {
                                    newEvent = new GEDCOMIndividualEvent(aBase.Tree, record, "", "");
                                }
                                else
                                {
                                    newEvent = new GEDCOMFamilyEvent(aBase.Tree, record, "", "");
                                }
                            }

                            fmEventEdit.Event = newEvent;
                            DialogResult dialogResult = TfmGEDKeeper.Instance.ShowModalEx(fmEventEdit, true);

                            if (dialogResult != DialogResult.OK)
                            {
                                if (dialogResult == DialogResult.Cancel)
                                {
                                    if (aEvent == null)
                                    {
                                        newEvent.Dispose();
                                    }
                                }
                            }
                            else
                            {
                                newEvent = fmEventEdit.Event;

                                if (aEvent == null)
                                {
                                    if (record is GEDCOMIndividualRecord)
                                    {
                                        (record as GEDCOMIndividualRecord).AddIndividualEvent(newEvent);
                                    }
                                    else
                                    {
                                        (record as GEDCOMFamilyRecord).FamilyEvents.Add(newEvent as GEDCOMFamilyEvent);
                                    }
                                }
                                else
                                {
                                    if (record is GEDCOMIndividualRecord && newEvent != aEvent)
                                    {
                                        (record as GEDCOMIndividualRecord).IndividualEvents.DeleteObject(aEvent);
                                        (record as GEDCOMIndividualRecord).AddIndividualEvent(newEvent);
                                    }
                                }

                                aEvent = newEvent;
                                result = true;
                            }
                        }
                        break;

                    case RecordAction.raDelete:
                        if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_RemoveEventQuery)) != DialogResult.No) {
                            if (record is GEDCOMIndividualRecord) {
                                (record as GEDCOMIndividualRecord).IndividualEvents.DeleteObject(aEvent);
                            } else {
                                (record as GEDCOMFamilyRecord).FamilyEvents.DeleteObject(aEvent as GEDCOMFamilyEvent);
                            }

                            aEvent = null;
                            result = true;
                        }
                        break;

                    case RecordAction.raMoveUp:
                    case RecordAction.raMoveDown:
                        if (record is GEDCOMIndividualRecord)
                        {
                            GEDCOMIndividualRecord iRec = record as GEDCOMIndividualRecord;
                            int idx = iRec.IndividualEvents.IndexOfObject(aEvent);
                            switch (action)
                            {
                                case RecordAction.raMoveUp:
                                    iRec.IndividualEvents.Exchange(idx - 1, idx);
                                    break;

                                case RecordAction.raMoveDown:
                                    iRec.IndividualEvents.Exchange(idx, idx + 1);
                                    break;
                            }
                            result = true;
                        }
                        else
                        {
                            GEDCOMFamilyRecord fRec = record as GEDCOMFamilyRecord;
                            int idx = fRec.FamilyEvents.IndexOfObject(aEvent as GEDCOMFamilyEvent);
                            switch (action)
                            {
                                case RecordAction.raMoveUp:
                                    fRec.FamilyEvents.Exchange(idx - 1, idx);
                                    break;

                                case RecordAction.raMoveDown:
                                    fRec.FamilyEvents.Exchange(idx, idx + 1);
                                    break;
                            }
                            result = true;
                        }
                        break;
                }

                if (result) aBase.Modified = true;
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKEventsSheet.ModifyRecEvent(): " + ex.Message);
                return false;
            }

            return result;
        }

    }
}
