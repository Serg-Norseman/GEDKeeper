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
            fView.EventType.Activate();
        }

        public override bool Accept()
        {
            try {
                fEvent.Place.StringValue = fView.Place.Text;
                fBase.Context.Tree.SetPtrValue(fEvent.Place.Location, fTempLocation);
                fEvent.Classification = fView.EventName.Text;
                fEvent.Cause = fView.Cause.Text;
                fEvent.Agency = fView.Agency.Text;

                GDMCustomDate dt = fView.Date.Date;
                if (dt == null) throw new ArgumentNullException("dt");

                fEvent.Date.ParseString(dt.StringValue);

                int eventType = fView.EventType.GetSelectedTag<int>();
                if (fEvent is GDMFamilyEvent) {
                    fEvent.SetName(GKData.FamilyEvents[eventType].Sign);
                } else {
                    GKData.EventStruct eventProps = GKData.PersonEvents[eventType];
                    fEvent.SetName(eventProps.Sign);
                    fEvent.StringValue = (eventProps.Kind == PersonEventKind.ekFact) ? fView.Attribute.Text : string.Empty;
                }

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

        private void SetEventTypes(GKData.EventStruct[] eventTypes)
        {
            fView.EventType.Sorted = false;

            fView.EventType.Clear();
            int num = eventTypes.Length;
            for (int i = 0; i < num; i++) {
                fView.EventType.AddItem(LangMan.LS(eventTypes[i].Name), i);
            }

            fView.EventType.Sorted = true;
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
    }
}
