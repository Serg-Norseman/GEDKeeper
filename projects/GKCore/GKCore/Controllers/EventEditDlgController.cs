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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Themes;

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
                    AppHost.EventDefinitions.Collect(fEvent);
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

        public override void Done()
        {
            fView.NotesList.ListModel.SaveSettings();
            fView.MediaList.ListModel.SaveSettings();
            fView.SourcesList.ListModel.SaveSettings();
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

                var eventDef = fView.EventType.GetSelectedTag<EventDef>();
                if (eventDef.Kind == EventKind.ekFact) {
                    var attrValue = fView.Attribute.Text;

                    if (string.IsNullOrEmpty(attrValue) && !eventDef.AcceptableEmpty) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FactValueIsInvalid));
                        throw new Exception();
                    }

                    fEvent.StringValue = attrValue;
                } else {
                    fEvent.StringValue = string.Empty;
                }

                fEvent.SetName(eventDef.Tag);
                fEvent.Classification = eventDef.Type;

                string key = eventDef.Tag + ":" + eventDef.Type;
                fBase.Context.EventStats.Increment(key);

                fEvent.Place.StringValue = fView.Place.Text;
                fBase.Context.Tree.SetPtrValue(fEvent.Place.Location, fTempLocation);
                fEvent.Cause = fView.Cause.Text;
                fEvent.Agency = fView.Agency.Text;

                if (fEvent is GDMIndividualEvent && eventDef.Kind == EventKind.ekFact) {
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

        private void SetEventTypes(EventTarget target)
        {
            var freqList = new List<FreqItem<EventDef>>();
            var eventStats = fBase.Context.EventStats;

            var eventDefs = AppHost.EventDefinitions.List;
            for (int i = 0; i < eventDefs.Count; i++) {
                var evDef = eventDefs[i];

                if ((evDef.Target == target || evDef.Target == EventTarget.etAny) && evDef.Enabled) {
                    string key = evDef.Tag + ":" + evDef.Type;
                    int stat = eventStats.GetValue(key);

                    freqList.Add(new FreqItem<EventDef>(evDef, evDef.DisplayName, stat));
                }
            }

            FreqCollection<string>.PopulateCombo(fView.EventType, freqList, null);
        }

        public override void UpdateView()
        {
            fView.NotesList.ListModel.DataOwner = fEvent;
            fView.MediaList.ListModel.DataOwner = fEvent;
            fView.SourcesList.ListModel.DataOwner = fEvent;

            EventTarget evtTarget = fEvent is GDMFamilyEvent ? EventTarget.etFamily : EventTarget.etIndividual;
            SetEventTypes(evtTarget);

            var evDef = AppHost.EventDefinitions.Find(fEvent);
            fView.EventType.SetSelectedTag(evDef);
            if (evDef != null && evDef.Kind == EventKind.ekFact) {
                fView.Attribute.Text = fEvent.StringValue;
            }

            ChangeEventType();

            fView.Date.Date = fEvent.Date.Value;

            var causVals = fBase.Context.ValuesCollection.GetValues(GEDCOMTagName.CAUS);
            UpdateCombo(fView.Cause, true, false, causVals, fEvent.Cause);

            var agncVals = fBase.Context.ValuesCollection.GetValues(GEDCOMTagName.AGNC);
            UpdateCombo(fView.Agency, true, false, agncVals, fEvent.Agency);

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
            var evDef = fView.EventType.GetSelectedTag<EventDef>();
            if (evDef == null) {
                evDef = AppHost.EventDefinitions.Find(GEDCOMTagName.EVEN, "");
                fView.EventType.SetSelectedTag(evDef);
            }

            string evTag = evDef.Tag;
            bool isFact = (evDef.Kind == EventKind.ekFact);
            SetAttributeMode(isFact);

            string[] vals;
            bool canbeSorted, fixedList;
            if (evTag == GEDCOMTagName._BGRO) {
                vals = GKData.BloodGroups.Split('|');
                canbeSorted = false;
                fixedList = true;
            } else {
                string evKey = evDef.Tag + ":" + evDef.Type;
                vals = fBase.Context.ValuesCollection.GetValues(evKey);
                canbeSorted = true;
                fixedList = false;
            }
            UpdateCombo(fView.Attribute, canbeSorted, fixedList, vals, fView.Attribute.Text);
        }

        private void UpdateCombo(IComboBox combo, bool canbeSorted, bool readOnly, string[] values, string current)
        {
            combo.Clear();
            if (values != null) {
                combo.AddRange(values, canbeSorted);
            }
            combo.Text = current;
            combo.ReadOnly = readOnly;
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

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            GetControl<IButton>("btnPlaceAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            GetControl<IButton>("btnPlaceDelete").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);

            fView.NotesList.ApplyTheme();
            fView.MediaList.ApplyTheme();
            fView.SourcesList.ApplyTheme();
        }
    }
}
