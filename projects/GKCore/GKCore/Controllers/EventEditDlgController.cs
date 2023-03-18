/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NotesList.ListModel = new NoteLinksListModel(baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(baseWin, fLocalUndoman);
            fView.SourcesList.ListModel = new SourceCitationsListModel(baseWin, fLocalUndoman);
        }

        public override bool Accept()
        {
            try {
                try {
                    GDMCustomDate dt = fView.Date.Date;
                    if (dt == null) throw new ArgumentNullException("dt");

                    fEvent.Date.ParseString(dt.StringValue);
                } catch (Exception ex) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_DateInvalid));
                    throw ex;
                }

                fEvent.Place.StringValue = fView.Place.Text;
                fBase.Context.Tree.SetPtrValue(fEvent.Place.Location, fTempLocation);
                fEvent.Classification = fView.EventName.Text;
                fEvent.Cause = fView.Cause.Text;
                fEvent.Agency = fView.Agency.Text;

                string tagName;
                int eventType = fView.EventType.GetSelectedTag<int>();
                if (fEvent is GDMFamilyEvent) {
                    tagName = GKData.FamilyEvents[eventType].Sign;
                } else {
                    GKData.EventStruct eventProps = GKData.PersonEvents[eventType];
                    tagName = eventProps.Sign;
                    fEvent.StringValue = (eventProps.Kind == PersonEventKind.ekFact) ? fView.Attribute.Text : string.Empty;
                }
                fEvent.SetName(tagName);
                fBase.Context.IncrementEventStats(tagName);

                if (fEvent is GDMIndividualEvent) {
                    if (GKData.PersonEvents[eventType].Kind == PersonEventKind.ekFact) {
                        var attr = new GDMIndividualAttribute();
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

        private sealed class EventX
        {
            public string Name;
            public int Index;
            public int Stat;
            public bool Use;

            public EventX(string name, int index, int stat)
            {
                Name = name;
                Index = index;
                Stat = stat;
                Use = (stat > 0);
            }
        }

        public const string LineItem = " ------------------------------ ";

        private void SetEventTypes(GKData.EventStruct[] eventTypes)
        {
            var list = new List<EventX>();
            var eventStats = fBase.Context.EventStats;
            for (int i = 0; i < eventTypes.Length; i++) {
                int stat;
                if (!eventStats.TryGetValue(eventTypes[i].Sign, out stat)) {
                    stat = 0;
                }
                list.Add(new EventX(LangMan.LS(eventTypes[i].Name), i, stat));
            }
            list.Sort((x, y) => { return (-10 * x.Use.CompareTo(y.Use)) + x.Name.CompareTo(y.Name); });

            fView.EventType.Clear();
            bool use = false;
            for (int i = 0; i < list.Count; i++) {
                var item = list[i];
                if (use != item.Use && i != 0) {
                    fView.EventType.AddItem(LineItem, -1);
                }
                //fView.EventType.AddItem(string.Format("{0} [{1}]", item.Name, item.Stat), item.Index);
                fView.EventType.AddItem(item.Name, item.Index);
                use = item.Use;
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
            UpdatePlace();

            fView.NotesList.UpdateSheet();
            fView.MediaList.UpdateSheet();
            fView.SourcesList.UpdateSheet();
        }

        private void UpdatePlace()
        {
            if (fTempLocation != null) {
                fView.Place.Text = fTempLocation.LocationName;
                SetLocationMode(true);
            } else {
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

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_Event);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<IButton>("btnAddress").Text = LangMan.LS(LSID.LSID_Address) + @"...";
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.LSID_Common);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.LSID_RPMultimedia);
            GetControl<ITabPage>("pageSources").Text = LangMan.LS(LSID.LSID_RPSources);
            GetControl<ILabel>("lblEvent").Text = LangMan.LS(LSID.LSID_Event);
            GetControl<ILabel>("lblAttrValue").Text = LangMan.LS(LSID.LSID_Value);
            GetControl<ILabel>("lblPlace").Text = LangMan.LS(LSID.LSID_Place);
            GetControl<ILabel>("lblDate").Text = LangMan.LS(LSID.LSID_Date);
            GetControl<ILabel>("lblCause").Text = LangMan.LS(LSID.LSID_Cause);
            GetControl<ILabel>("lblOrg").Text = LangMan.LS(LSID.LSID_Agency);

            SetToolTip("btnPlaceAdd", LangMan.LS(LSID.LSID_PlaceAddTip));
            SetToolTip("btnPlaceDelete", LangMan.LS(LSID.LSID_PlaceDeleteTip));
        }
    }
}
