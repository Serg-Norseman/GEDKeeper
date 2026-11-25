/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Events;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Utilities;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class EventEditDlgController : DialogController<IEventEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnAddress;
        private IButton _btnPlaceAdd;
        private IButton _btnPlaceDelete;
        private IButton _btnAge;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageSources;
        private ITabPage _pageCommon;
        private ILabel _lblEvent;
        private ILabel _lblPlace;
        private ILabel _lblDate;
        private ILabel _lblCause;
        private ILabel _lblOrg;
        private ILabel _lblAttrValue;
        private IComboBox _cmbEventType;
        private ITextBox _txtEventPlace;
        private IComboBox _txtEventCause;
        private IComboBox _txtEventOrg;
        private IComboBox _txtAttribute;
        private IDateControl _dateCtl;
        private ILabel _lblAge;
        private ITextBox _txtAge;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
        private ISheetList _fSourcesList;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbEventType.Activate();
            _dateCtl.DateChanged += new EventHandler(dateCtl_DateChanged);

            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fMediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
            _fSourcesList.ListModel = new SourceCitationsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fNotesList.ListModel.SaveSettings();
            _fMediaList.ListModel.SaveSettings();
            _fSourcesList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                GDMCustomDate dt = _dateCtl.Date;
                if (dt == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DateInvalid));
                    return false;
                }
                fEvent.Date.ParseString(dt.StringValue);

                var eventDef = _cmbEventType.GetSelectedTag<EventDef>();
                if (eventDef.Kind == EventKind.ekFact) {
                    var attrValue = _txtAttribute.Text;

                    if (string.IsNullOrEmpty(attrValue) && !eventDef.AcceptableEmpty) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FactValueIsInvalid));
                        return false;
                    }

                    fEvent.StringValue = attrValue;
                } else {
                    fEvent.StringValue = string.Empty;
                }

                fEvent.SetName(eventDef.Tag);
                fEvent.Classification = eventDef.Type;

                string key = eventDef.Tag + ":" + eventDef.Type;
                fBase.Context.EventStats.Increment(key);

                fEvent.Place.StringValue = _txtEventPlace.Text;
                fBase.Context.Tree.SetPtrValue(fEvent.Place.Location, fTempLocation);
                fEvent.Cause = _txtEventCause.Text;
                fEvent.Agency = _txtEventOrg.Text;

                if (fEvent is GDMIndividualEvent && eventDef.Kind == EventKind.ekFact) {
                    var attr = new GDMIndividualAttribute();
                    attr.Assign(fEvent);
                    fEvent = attr;
                }

                if (!Validate(fEvent)) return false;

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("EventEditDlgController.Accept()", ex);
                return false;
            }
        }

        private void SetEventTypes(EventTarget target)
        {
            var freqList = BaseController.GetFrequencyEventTypes(fBase.Context, target);
            FreqCollection<string>.PopulateCombo(_cmbEventType, freqList, null);
        }

        public override void UpdateView()
        {
            _fNotesList.ListModel.DataOwner = fEvent;
            _fMediaList.ListModel.DataOwner = fEvent;
            _fSourcesList.ListModel.DataOwner = fEvent;

            EventTarget evtTarget = fEvent is GDMFamilyEvent ? EventTarget.etFamily : EventTarget.etIndividual;
            SetEventTypes(evtTarget);

            var evDef = AppHost.EventDefinitions.Find(fEvent);
            _cmbEventType.SetSelectedTag(evDef);
            if (evDef != null && evDef.Kind == EventKind.ekFact) {
                _txtAttribute.Text = fEvent.StringValue;
            }

            ChangeEventType();

            _dateCtl.Date = fEvent.Date.Value;

            var causVals = fBase.Context.ValuesCollection.GetValues(GEDCOMTagName.CAUS);
            UpdateCombo(_txtEventCause, true, false, causVals, fEvent.Cause);

            var agncVals = fBase.Context.ValuesCollection.GetValues(GEDCOMTagName.AGNC);
            UpdateCombo(_txtEventOrg, true, false, agncVals, fEvent.Agency);

            fTempLocation = fBase.Context.Tree.GetPtrValue<GDMLocationRecord>(fEvent.Place.Location);
            UpdatePlace(true);

            UpdateAge();
        }

        private void UpdatePlace(bool forced)
        {
            if (fTempLocation != null) {
                _txtEventPlace.Text = GKUtils.GetLocationNameExt(fTempLocation, _dateCtl.Date);
                SetLocationMode(true);
            } else if (forced) {
                _txtEventPlace.Text = fEvent.Place.StringValue;
                SetLocationMode(false);
            }
        }

        private void SetLocationMode(bool active)
        {
            bool disNoStd = GlobalOptions.Instance.DisableNonStdFeatures;

            if (active) {
                _txtEventPlace.ReadOnly = true;
                _btnPlaceAdd.Enabled = false;
                _btnPlaceDelete.Enabled = true && !disNoStd;
            } else {
                _txtEventPlace.ReadOnly = false;
                _btnPlaceAdd.Enabled = true && !disNoStd;
                _btnPlaceDelete.Enabled = false;
            }
        }

        public async void AddPlace()
        {
            fTempLocation = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtLocation, new object[] { _txtEventPlace.Text }) as GDMLocationRecord;
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

        private void UpdateAge()
        {
            _txtAge.Enabled = false;
            _txtAge.Text = GKUtils.GetAgeDisplayStr(fEvent);
        }

        public async void ModifyAge()
        {
            var result = await BaseController.ModifyAge(fView, fBase, fEvent);
            if (result) {
                UpdateAge();
            }
        }

        private void SetAttributeMode(bool active)
        {
            if (active) {
                _txtAttribute.Enabled = true;
                //txtAttribute.BackColor = SystemColors.Window;
            } else {
                _txtAttribute.Enabled = false;
                //txtAttribute.BackColor = SystemColors.Control;
                _txtAttribute.Text = "";
            }
        }

        public void ChangeEventType()
        {
            var evDef = _cmbEventType.GetSelectedTag<EventDef>();
            if (evDef == null) {
                evDef = AppHost.EventDefinitions.Find(GEDCOMTagName.EVEN, "");
                _cmbEventType.SetSelectedTag(evDef);
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
            UpdateCombo(_txtAttribute, canbeSorted, fixedList, vals, _txtAttribute.Text);
        }

        private static void UpdateCombo(IComboBox combo, bool canbeSorted, bool readOnly, string[] values, string current)
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
                _dateCtl.PasteValue(data);
            }
        }

        private void dateCtl_DateChanged(object sender, System.EventArgs e)
        {
            UpdatePlace(false);
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Event));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _btnAddress.Text = LangMan.LS(LSID.Address) + @"...";
            _pageCommon.Text = LangMan.LS(LSID.Common);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);
            _pageSources.Text = LangMan.LS(LSID.RPSources);
            _lblEvent.Text = LangMan.LS(LSID.Event);
            _lblAttrValue.Text = LangMan.LS(LSID.Value);
            _lblPlace.Text = LangMan.LS(LSID.Place);
            _lblDate.Text = LangMan.LS(LSID.Date);
            _lblCause.Text = LangMan.LS(LSID.Cause);
            _lblOrg.Text = LangMan.LS(LSID.Agency);
            _btnAge.Text = LangMan.LS(LSID.Age);
            _lblAge.Text = LangMan.LS(LSID.Age);

            SetToolTip("btnPlaceAdd", LangMan.LS(LSID.PlaceAddTip));
            SetToolTip("btnPlaceDelete", LangMan.LS(LSID.PlaceDeleteTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnPlaceAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnPlaceDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
        }
    }
}
