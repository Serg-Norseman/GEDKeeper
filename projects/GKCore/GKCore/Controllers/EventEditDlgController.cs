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
using System.Collections.Generic;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
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
            fView.EventType.Activate();
            fView.Date.DateChanged += new EventHandler(dateCtl_DateChanged);
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
            fView.SourcesList.ListModel = new SourceCitationsListModel(fView, baseWin, fLocalUndoman);
        }

        public override bool Accept()
        {
            try {
                try {
                    GDMCustomDate dt = fView.Date.Date;
                    if (dt == null) throw new ArgumentNullException("dt");

                    fEvent.Date.ParseString(dt.StringValue);
                } catch (Exception ex) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DateInvalid));
                    throw ex;
                }

                int eventType = fView.EventType.GetSelectedTag<int>();
                var eventProps = (fEvent is GDMFamilyEvent) ? GKData.FamilyEvents[eventType] : GKData.PersonEvents[eventType];

                if (eventProps.Kind == PersonEventKind.ekFact) {
                    var attrValue = fView.Attribute.Text;

                    if (string.IsNullOrEmpty(attrValue) && !eventProps.AcceptableEmpty) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FactValueIsInvalid));
                        throw new Exception();
                    }

                    fEvent.StringValue = attrValue;
                } else {
                    fEvent.StringValue = string.Empty;
                }

                fEvent.Place.StringValue = fView.Place.Text;
                fBase.Context.Tree.SetPtrValue(fEvent.Place.Location, fTempLocation);
                fEvent.Classification = fView.EventName.Text;
                fEvent.Cause = fView.Cause.Text;
                fEvent.Agency = fView.Agency.Text;

                string tagName = eventProps.Sign;
                fEvent.SetName(tagName);
                fBase.Context.EventStats.Increment(tagName);

                if (fEvent is GDMIndividualEvent && eventProps.Kind == PersonEventKind.ekFact) {
                    var attr = new GDMIndividualAttribute();
                    attr.Assign(fEvent);
                    fEvent = attr;
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
            var freqList = new List<FreqItem<int>>();

            var eventStats = fBase.Context.EventStats;
            for (int i = 0; i < eventTypes.Length; i++) {
                int stat = eventStats.GetValue(eventTypes[i].Sign);
                freqList.Add(new FreqItem<int>(i, LangMan.LS(eventTypes[i].Name), stat));
            }

            FreqCollection<string>.PopulateCombo(fView.EventType, freqList, -1);
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
                fView.EventType.SetSelectedTag(idx);
            } else {
                SetEventTypes(GKData.PersonEvents);
                int idx = GKUtils.GetPersonEventIndex(evtName);
                if (idx < 0) idx = 0;
                fView.EventType.SetSelectedTag(idx);

                if (idx >= 0 && GKData.PersonEvents[idx].Kind == PersonEventKind.ekFact) {
                    fView.Attribute.Text = fEvent.StringValue;
                }
            }

            ChangeEventType();

            fView.Date.Date = fEvent.Date.Value;
            fView.EventName.Text = fEvent.Classification;
            fView.Cause.Text = fEvent.Cause;
            fView.Agency.Text = fEvent.Agency;

            fTempLocation = fBase.Context.Tree.GetPtrValue<GDMLocationRecord>(fEvent.Place.Location);
            UpdatePlace(true);

            fView.NotesList.UpdateSheet();
            fView.MediaList.UpdateSheet();
            fView.SourcesList.UpdateSheet();
        }

        private void UpdatePlace(bool forced)
        {
            if (fTempLocation != null) {
                fView.Place.Text = GKUtils.GetLocationNameExt(fTempLocation, fView.Date.Date);
                SetLocationMode(true);
            } else if (forced) {
                fView.Place.Text = fEvent.Place.StringValue;
                SetLocationMode(false);
            }
        }

        private void SetLocationMode(bool active)
        {
            if (active) {
                fView.Place.ReadOnly = true;
                //txtEventPlace.BackColor = SystemColors.Control;
                GetControl<IButton>("btnPlaceAdd").Enabled = false;
                GetControl<IButton>("btnPlaceDelete").Enabled = true;
            } else {
                fView.Place.ReadOnly = false;
                //txtEventPlace.BackColor = SystemColors.Window;
                GetControl<IButton>("btnPlaceAdd").Enabled = true;
                GetControl<IButton>("btnPlaceDelete").Enabled = false;
            }
        }

        public async void AddPlace()
        {
            fTempLocation = await fBase.Context.SelectRecord(fView, GDMRecordType.rtLocation, new object[] { fView.Place.Text }) as GDMLocationRecord;
            UpdatePlace(true);
        }

        public void RemovePlace()
        {
            fTempLocation = null;
            UpdatePlace(true);
        }

        public async void ModifyAddress()
        {
            await BaseController.ModifyAddress(fView, fBase, fEvent.Address);
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
            int idx = fView.EventType.GetSelectedTag<int>();
            if (idx < 0) {
                idx = 0;
                fView.EventType.SetSelectedTag(idx);
            }

            if (fEvent is GDMFamilyEvent) {
                SetAttributeMode(false);
            } else {
                if (idx >= 0) {
                    if (GKData.PersonEvents[idx].Kind == PersonEventKind.ekEvent) {
                        SetAttributeMode(false);
                    } else {
                        SetAttributeMode(true);
                    }
                }
            }

            string evName;
            if (fEvent is GDMFamilyEvent) {
                evName = GKData.FamilyEvents[idx].Sign;
            } else {
                evName = GKData.PersonEvents[idx].Sign;
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
                fView.Attribute.AddRange(vals, canbeSorted);
                fView.Attribute.Text = tmp;
                fView.Attribute.ReadOnly = (!userInput);
            }
        }

        public void SendData(string signature, string data)
        {
            if (signature == "event_year" && !string.IsNullOrEmpty(data)) {
                var dateControl = fView.Date;
                dateControl.PasteValue(data);
            }
        }

        private void dateCtl_DateChanged(object sender, System.EventArgs e)
        {
            UpdatePlace(false);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Event);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<IButton>("btnAddress").Text = LangMan.LS(LSID.Address) + @"...";
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.Common);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.RPMultimedia);
            GetControl<ITabPage>("pageSources").Text = LangMan.LS(LSID.RPSources);
            GetControl<ILabel>("lblEvent").Text = LangMan.LS(LSID.Event);
            GetControl<ILabel>("lblAttrValue").Text = LangMan.LS(LSID.Value);
            GetControl<ILabel>("lblPlace").Text = LangMan.LS(LSID.Place);
            GetControl<ILabel>("lblDate").Text = LangMan.LS(LSID.Date);
            GetControl<ILabel>("lblCause").Text = LangMan.LS(LSID.Cause);
            GetControl<ILabel>("lblOrg").Text = LangMan.LS(LSID.Agency);

            SetToolTip("btnPlaceAdd", LangMan.LS(LSID.PlaceAddTip));
            SetToolTip("btnPlaceDelete", LangMan.LS(LSID.PlaceDeleteTip));
        }
    }
}
