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
using BSLib;
using GDModel;
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
    public sealed class PersonsFilterDlgController : DialogController<IPersonsFilterDlg>
    {
        private readonly IndividualListModel fListMan;

        public PersonsFilterDlgController(IPersonsFilterDlg view, IRecordsListModel listMan) : base(view)
        {
            fListMan = (IndividualListModel)listMan;
        }

        public void RemoveFilter(IComboBox sender)
        {
            if (sender == null) return;

            GlobalOptions options = GlobalOptions.Instance;

            if (sender == fView.NameCombo) {
                GKUtils.RemoveFilter(sender.Text, options.NameFilters);
                UpdateFiltersCombo(sender, options.NameFilters);
            }

            if (sender == fView.ResidenceCombo) {
                GKUtils.RemoveFilter(sender.Text, options.ResidenceFilters);
                UpdateFiltersCombo(sender, options.ResidenceFilters);
            }

            if (sender == fView.EventValCombo) {
                GKUtils.RemoveFilter(sender.Text, options.EventFilters);
                UpdateFiltersCombo(sender, options.EventFilters);
            }

            sender.Text = "*";
        }

        public override bool Accept()
        {
            try {
                IndividualListFilter iFilter = (IndividualListFilter)fListMan.Filter;

                GKUtils.SaveFilter(fView.NameCombo.Text, GlobalOptions.Instance.NameFilters);
                GKUtils.SaveFilter(fView.ResidenceCombo.Text, GlobalOptions.Instance.ResidenceFilters);
                GKUtils.SaveFilter(fView.EventValCombo.Text, GlobalOptions.Instance.EventFilters);

                iFilter.PatriarchOnly = fView.OnlyPatriarchsCheck.Checked;

                int lifeSel = fView.GetLifeRadio();
                if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked) {
                    iFilter.AliveBeforeDate = fView.AliveBeforeDate.Text;
                    iFilter.FilterLifeMode = (FilterLifeMode)lifeSel;
                    if (iFilter.FilterLifeMode == FilterLifeMode.lmAliveBefore) {
                        try {
                            /*DateTime dt = */
                            DateTime.Parse(fView.AliveBeforeDate.Text);
                        } catch {
                            AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DateInvalid));
                            return false;
                        }
                    }
                }

                iFilter.Sex = (GDMSex)fView.GetSexRadio();

                if (fView.NameCombo.Text == "") fView.NameCombo.Text = @"*";
                iFilter.Name = fView.NameCombo.Text;

                if (fView.ResidenceCombo.Text == "") fView.ResidenceCombo.Text = @"*";
                iFilter.Residence = fView.ResidenceCombo.Text;

                if (fView.EventValCombo.Text == "") fView.EventValCombo.Text = @"*";
                iFilter.EventVal = fView.EventValCombo.Text;

                int selectedIndex = fView.GroupCombo.SelectedIndex;
                if (selectedIndex >= 0 && selectedIndex < 3) {
                    iFilter.FilterGroupMode = (FilterGroupMode)fView.GroupCombo.SelectedIndex;
                    iFilter.GroupRef = "";
                } else {
                    GDMRecord rec = fView.GroupCombo.GetSelectedTag<GDMRecord>();
                    if (rec != null) {
                        iFilter.FilterGroupMode = FilterGroupMode.Selected;
                        iFilter.GroupRef = rec.XRef;
                    } else {
                        iFilter.FilterGroupMode = FilterGroupMode.All;
                        iFilter.GroupRef = "";
                    }
                }

                selectedIndex = fView.SourceCombo.SelectedIndex;
                if (selectedIndex >= 0 && selectedIndex < 3) {
                    iFilter.SourceMode = (FilterGroupMode)fView.SourceCombo.SelectedIndex;
                    iFilter.SourceRef = "";
                } else {
                    GDMRecord rec = fView.SourceCombo.GetSelectedTag<GDMRecord>();
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
            IndividualListFilter iFilter = (IndividualListFilter)fListMan.Filter;
            GlobalOptions options = GlobalOptions.Instance;

            UpdateFiltersCombo(fView.NameCombo, options.NameFilters);
            UpdateFiltersCombo(fView.ResidenceCombo, options.ResidenceFilters);
            UpdateFiltersCombo(fView.EventValCombo, options.EventFilters);

            int lifeSel;
            if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked) {
                lifeSel = (int)iFilter.FilterLifeMode;
                fView.SetLifeEnabled(true);
                fView.AliveBeforeDate.Text = iFilter.AliveBeforeDate;
            } else {
                lifeSel = -1;
                fView.SetLifeEnabled(false);
                fView.AliveBeforeDate.Text = "";
            }

            fView.SetLifeRadio(lifeSel);
            fView.SetSexRadio((int)iFilter.Sex);

            fView.NameCombo.Text = iFilter.Name;
            fView.ResidenceCombo.Text = iFilter.Residence;
            fView.EventValCombo.Text = iFilter.EventVal;
            fView.OnlyPatriarchsCheck.Checked = iFilter.PatriarchOnly;

            GDMTree tree = Base.Context.Tree;

            fView.GroupCombo.Clear();
            fView.GroupCombo.AddItem<GDMRecord>(LangMan.LS(LSID.SrcAll), null);
            fView.GroupCombo.AddItem<GDMRecord>(LangMan.LS(LSID.SrcNot), null);
            fView.GroupCombo.AddItem<GDMRecord>(LangMan.LS(LSID.SrcAny), null);
            var groups = GKUtils.GetGroups(tree);
            foreach (var item in groups) {
                fView.GroupCombo.AddItem<GDMRecord>(item.GroupName, item);
            }

            if (iFilter.FilterGroupMode != FilterGroupMode.Selected) {
                fView.GroupCombo.SelectedIndex = (int)iFilter.FilterGroupMode;
            } else {
                var groupRec = tree.FindXRef<GDMGroupRecord>(iFilter.GroupRef);
                if (groupRec != null) fView.GroupCombo.Text = groupRec.GroupName;
            }

            fView.SourceCombo.Clear();
            fView.SourceCombo.AddItem<GDMRecord>(LangMan.LS(LSID.SrcAll), null);
            fView.SourceCombo.AddItem<GDMRecord>(LangMan.LS(LSID.SrcNot), null);
            fView.SourceCombo.AddItem<GDMRecord>(LangMan.LS(LSID.SrcAny), null);
            var sources = GKUtils.GetSources(tree);
            foreach (var item in sources) {
                fView.SourceCombo.AddItem<GDMRecord>(item.ShortTitle, item);
            }

            if (iFilter.SourceMode != FilterGroupMode.Selected) {
                fView.SourceCombo.SelectedIndex = (int)iFilter.SourceMode;
            } else {
                var sourceRec = tree.FindXRef<GDMSourceRecord>(iFilter.SourceRef);
                if (sourceRec != null) fView.SourceCombo.Text = sourceRec.ShortTitle;
            }
        }

        public override void SetLocale()
        {
            GetControl<ITabPage>("pageSpecificFilter").Text = LangMan.LS(LSID.PersonsFilter);
            GetControl<IRadioButton>("rbAll").Text = LangMan.LS(LSID.All);
            GetControl<IRadioButton>("rbOnlyLive").Text = LangMan.LS(LSID.OnlyAlive);
            GetControl<IRadioButton>("rbOnlyDead").Text = LangMan.LS(LSID.OnlyDied);
            GetControl<IRadioButton>("rbAliveBefore").Text = LangMan.LS(LSID.AliveBefore).ToLower();
            GetControl<IRadioButton>("rbSexAll").Text = LangMan.LS(LSID.All);
            GetControl<IRadioButton>("rbSexMale").Text = LangMan.LS(LSID.OnlyMans);
            GetControl<IRadioButton>("rbSexFemale").Text = LangMan.LS(LSID.OnlyWomans);
            GetControl<ILabel>("lblAliveBefore").Text = LangMan.LS(LSID.AliveBefore) + ":";
            GetControl<ILabel>("lblNameMask").Text = LangMan.LS(LSID.NameMask);
            GetControl<ILabel>("lblPlaceMask").Text = LangMan.LS(LSID.PlaceMask);
            GetControl<ILabel>("lblEventsMask").Text = LangMan.LS(LSID.EventMask);
            GetControl<ILabel>("lblGroups").Text = LangMan.LS(LSID.RPGroups);
            GetControl<ILabel>("lblSources").Text = LangMan.LS(LSID.RPSources);
            GetControl<ICheckBox>("chkOnlyPatriarchs").Text = LangMan.LS(LSID.OnlyPatriarchs);
        }

        public override void ApplyTheme()
        {
            // dummy
        }
    }
}
