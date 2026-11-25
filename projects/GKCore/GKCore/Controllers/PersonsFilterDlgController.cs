/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonsFilterDlgController : CommonFilterDlgController<IPersonsFilterDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IComboBox _cmbEventVal;
        private IComboBox _cmbSource;
        private IComboBox _cmbGroup;
        private IComboBox _cmbResidence;
        private ITextBox _txtAliveBeforeDate;
        private IRadioButton _rbSexMale;
        private IRadioButton _rbSexAll;
        private IRadioButton _rbSexFemale;
        private IGroupBox _rgSex;
        private IComboBox _txtName;
        private IRadioButton _rbAll;
        private IRadioButton _rbOnlyLive;
        private IRadioButton _rbOnlyDead;
        private IRadioButton _rbAliveBefore;
        private IGroupBox _rgLife;
        private ILabel _lblEventsMask;
        private ILabel _lblSources;
        private ICheckBox _chkOnlyPatriarchs;
        private ILabel _lblGroups;
        private ILabel _lblPlaceMask;
        private ILabel _lblNameMask;
        private ITabPage _pageSpecificFilter;
#pragma warning restore CS0649, IDE0044
        #endregion

        private readonly IndividualListModel fIndiListMan;
        private readonly GlobalOptions fOptions;

        public PersonsFilterDlgController(IPersonsFilterDlg view, IRecordsListModel listMan) : base(view, listMan)
        {
            fIndiListMan = (IndividualListModel)listMan;
            fOptions = GlobalOptions.Instance;
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbGroup.ReadOnly = true;
            _cmbSource.ReadOnly = true;
        }

        public void RemoveFilter(IComboBox sender)
        {
            if (sender == null) return;

            if (sender == _txtName) {
                GKUtils.RemoveFilter(sender.Text, fOptions.NameFilters);
                UpdateFiltersCombo(sender, fOptions.NameFilters);
            }

            if (sender == _cmbResidence) {
                GKUtils.RemoveFilter(sender.Text, fOptions.ResidenceFilters);
                UpdateFiltersCombo(sender, fOptions.ResidenceFilters);
            }

            if (sender == _cmbEventVal) {
                GKUtils.RemoveFilter(sender.Text, fOptions.EventFilters);
                UpdateFiltersCombo(sender, fOptions.EventFilters);
            }

            sender.Text = "*";
        }

        public override bool Accept()
        {
            try {
                if (!base.Accept())
                    return false;

                IndividualListFilter iFilter = (IndividualListFilter)fIndiListMan.Filter;

                iFilter.Name = GKUtils.SaveFilter(_txtName.Text, fOptions.NameFilters);
                iFilter.Residence = GKUtils.SaveFilter(_cmbResidence.Text, fOptions.ResidenceFilters);
                iFilter.EventVal = GKUtils.SaveFilter(_cmbEventVal.Text, fOptions.EventFilters);

                iFilter.Sex = (GDMSex)GetSexRadio();
                iFilter.PatriarchOnly = _chkOnlyPatriarchs.Checked;

                int lifeSel = GetLifeRadio();
                if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked) {
                    iFilter.AliveBeforeDate = _txtAliveBeforeDate.Text;
                    iFilter.FilterLifeMode = (FilterLifeMode)lifeSel;
                    if (iFilter.FilterLifeMode == FilterLifeMode.lmAliveBefore) {
                        try {
                            /*DateTime dt = */
                            DateTime.Parse(_txtAliveBeforeDate.Text);
                        } catch {
                            AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DateInvalid));
                            return false;
                        }
                    }
                }

                int selectedIndex = _cmbGroup.SelectedIndex;
                if (selectedIndex >= 0 && selectedIndex < 3) {
                    iFilter.FilterGroupMode = (FilterGroupMode)selectedIndex;
                    iFilter.GroupRef = "";
                } else {
                    GDMRecord rec = _cmbGroup.GetSelectedTag<GDMRecord>();
                    if (rec != null) {
                        iFilter.FilterGroupMode = FilterGroupMode.Selected;
                        iFilter.GroupRef = rec.XRef;
                    } else {
                        iFilter.FilterGroupMode = FilterGroupMode.All;
                        iFilter.GroupRef = "";
                    }
                }

                selectedIndex = _cmbSource.SelectedIndex;
                if (selectedIndex >= 0 && selectedIndex < 3) {
                    iFilter.SourceMode = (FilterGroupMode)selectedIndex;
                    iFilter.SourceRef = "";
                } else {
                    GDMRecord rec = _cmbSource.GetSelectedTag<GDMRecord>();
                    if (rec != null) {
                        iFilter.SourceMode = FilterGroupMode.Selected;
                        iFilter.SourceRef = rec.XRef;
                    } else {
                        iFilter.SourceMode = FilterGroupMode.All;
                        iFilter.SourceRef = "";
                    }
                }

                return true;
            } catch (Exception ex) {
                Logger.WriteError("PersonsFilterDlgController.Accept()", ex);
                return false;
            }
        }

        private void UpdateFiltersCombo(IComboBox comboBox, StringList filters)
        {
            comboBox.Clear();
            comboBox.Add("*");
            comboBox.AddStrings(filters);
            comboBox.ReadOnly = false;
        }

        public override void UpdateView()
        {
            base.UpdateView();

            IndividualListFilter iFilter = (IndividualListFilter)fIndiListMan.Filter;

            UpdateFiltersCombo(_txtName, fOptions.NameFilters);
            UpdateFiltersCombo(_cmbResidence, fOptions.ResidenceFilters);
            UpdateFiltersCombo(_cmbEventVal, fOptions.EventFilters);

            int lifeSel;
            if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked) {
                lifeSel = (int)iFilter.FilterLifeMode;
                SetLifeEnabled(true);
                _txtAliveBeforeDate.Text = iFilter.AliveBeforeDate;
            } else {
                lifeSel = -1;
                SetLifeEnabled(false);
                _txtAliveBeforeDate.Text = "";
            }

            SetLifeRadio(lifeSel);
            SetSexRadio((int)iFilter.Sex);

            _txtName.Text = iFilter.Name;
            _cmbResidence.Text = iFilter.Residence;
            _cmbEventVal.Text = iFilter.EventVal;
            _chkOnlyPatriarchs.Checked = iFilter.PatriarchOnly;

            GDMTree tree = Base.Context.Tree;

            _cmbGroup.Clear();
            _cmbGroup.AddItem<GDMRecord>(LangMan.LS(LSID.SrcAll), null);
            _cmbGroup.AddItem<GDMRecord>(LangMan.LS(LSID.SrcNot), null);
            _cmbGroup.AddItem<GDMRecord>(LangMan.LS(LSID.SrcAny), null);
            var groups = GKUtils.GetGroups(tree);
            foreach (var item in groups) {
                _cmbGroup.AddItem<GDMRecord>(item.GroupName, item);
            }

            if (iFilter.FilterGroupMode != FilterGroupMode.Selected) {
                _cmbGroup.SelectedIndex = (int)iFilter.FilterGroupMode;
            } else {
                var groupRec = tree.FindXRef<GDMGroupRecord>(iFilter.GroupRef);
                if (groupRec != null) _cmbGroup.Text = groupRec.GroupName;
            }

            _cmbSource.Clear();
            _cmbSource.AddItem<GDMRecord>(LangMan.LS(LSID.SrcAll), null);
            _cmbSource.AddItem<GDMRecord>(LangMan.LS(LSID.SrcNot), null);
            _cmbSource.AddItem<GDMRecord>(LangMan.LS(LSID.SrcAny), null);
            var sources = GKUtils.GetSources(tree);
            foreach (var item in sources) {
                _cmbSource.AddItem<GDMRecord>(GetSourceComfortableName(item), item);
            }

            if (iFilter.SourceMode != FilterGroupMode.Selected) {
                _cmbSource.SelectedIndex = (int)iFilter.SourceMode;
            } else {
                var sourceRec = tree.FindXRef<GDMSourceRecord>(iFilter.SourceRef);
                if (sourceRec != null) _cmbSource.Text = GetSourceComfortableName(sourceRec);
            }
        }

        private string GetSourceComfortableName(GDMSourceRecord sourceRecord)
        {
            if (fOptions.ConvenientSourcesFormat) {
                string strDate = string.Empty;
                var date = sourceRecord.Date.Value;
                if (date != null) {
                    strDate = date.GetDisplayString(fOptions.DefDateFormat, false, false, true);
                    if (!string.IsNullOrEmpty(strDate)) {
                        strDate = string.Format(", {0}", strDate);
                    }
                }
                string strRepo = string.Empty;
                string repositories = GKUtils.GetSourceRepositories(fBase.Context.Tree, sourceRecord);
                if (!string.IsNullOrEmpty(repositories)) {
                    strRepo = string.Format(", {0}", repositories);
                }
                return string.Format("{0}{1}{2}", sourceRecord.ShortTitle, strDate, strRepo);
            }
            return sourceRecord.ShortTitle;
        }

        private void SetLifeRadio(int lifeSel)
        {
            switch (lifeSel) {
                case 0:
                    _rbAll.Checked = true;
                    break;
                case 1:
                    _rbOnlyLive.Checked = true;
                    break;
                case 2:
                    _rbOnlyDead.Checked = true;
                    break;
                case 3:
                    _rbAliveBefore.Checked = true;
                    break;
            }
        }

        private void SetSexRadio(int sexSel)
        {
            switch (sexSel) {
                case 0:
                    _rbSexAll.Checked = true;
                    break;
                case 1:
                    _rbSexMale.Checked = true;
                    break;
                case 2:
                    _rbSexFemale.Checked = true;
                    break;
            }
        }

        private int GetLifeRadio()
        {
            int lifeSel = 0;
            if (_rbAll.Checked) lifeSel = 0;
            if (_rbOnlyLive.Checked) lifeSel = 1;
            if (_rbOnlyDead.Checked) lifeSel = 2;
            if (_rbAliveBefore.Checked) lifeSel = 3;
            return lifeSel;
        }

        private int GetSexRadio()
        {
            int sexSel = 0;
            if (_rbSexAll.Checked) sexSel = 0;
            if (_rbSexMale.Checked) sexSel = 1;
            if (_rbSexFemale.Checked) sexSel = 2;
            return sexSel;
        }

        private void SetLifeEnabled(bool value)
        {
            _rgLife.Enabled = value;
        }

        public override void SetLocale()
        {
            base.SetLocale();

            _pageSpecificFilter.Text = LangMan.LS(LSID.PersonsFilter);
            _rbAll.Text = LangMan.LS(LSID.All);
            _rbOnlyLive.Text = LangMan.LS(LSID.OnlyAlive);
            _rbOnlyDead.Text = LangMan.LS(LSID.OnlyDied);
            _rbAliveBefore.Text = LangMan.LS(LSID.AliveBefore).ToLower();
            _rbSexAll.Text = LangMan.LS(LSID.All);
            _rbSexMale.Text = LangMan.LS(LSID.OnlyMans);
            _rbSexFemale.Text = LangMan.LS(LSID.OnlyWomans);
            _lblNameMask.Text = LangMan.LS(LSID.NameMask);
            _lblPlaceMask.Text = LangMan.LS(LSID.PlaceMask);
            _lblEventsMask.Text = LangMan.LS(LSID.EventMask);
            _lblGroups.Text = LangMan.LS(LSID.RPGroups);
            _lblSources.Text = LangMan.LS(LSID.RPSources);
            _chkOnlyPatriarchs.Text = LangMan.LS(LSID.OnlyPatriarchs);
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
        }
    }
}
