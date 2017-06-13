/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class PersonsFilterDlg : CommonFilterDlg
    {
        private readonly IndividualListMan fListMan;

        public PersonsFilterDlg() : base()
        {
            InitializeComponent();
        }

        public PersonsFilterDlg(IBaseWindow baseWin, IListManager listMan) : base(baseWin, listMan)
        {
            InitializeComponent();

            SetSpecificLang();

            fListMan = (IndividualListMan)listMan;
            UpdateSpecific();

            // platform: in Mono tsSpecificFilter has 0 index, somehow
            #if __MonoCS__
            tabsFilters.Controls.SetChildIndex(pageSpecificFilter, 1);
            #endif

            tabsFilters.SelectedIndex = 1;
        }

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        public void SetSpecificLang()
        {
            Title = LangMan.LS(LSID.LSID_MIFilter);
            pageSpecificFilter.Text = LangMan.LS(LSID.LSID_PersonsFilter);
            rbAll.Text = LangMan.LS(LSID.LSID_All);
            rbOnlyLive.Text = LangMan.LS(LSID.LSID_OnlyAlive);
            rbOnlyDead.Text = LangMan.LS(LSID.LSID_OnlyDied);
            rbAliveBefore.Text = LangMan.LS(LSID.LSID_AliveBefore).ToLower();
            rbSexAll.Text = LangMan.LS(LSID.LSID_All);
            rbSexMale.Text = LangMan.LS(LSID.LSID_OnlyMans);
            rbSexFemale.Text = LangMan.LS(LSID.LSID_OnlyWomans);
            lblAliveBefore.Text = LangMan.LS(LSID.LSID_AliveBefore) + ':';
            lblNameMask.Text = LangMan.LS(LSID.LSID_NameMask);
            lblPlaceMask.Text = LangMan.LS(LSID.LSID_PlaceMask);
            lblEventsMask.Text = LangMan.LS(LSID.LSID_EventMask);
            lblGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
            lblSources.Text = LangMan.LS(LSID.LSID_RPSources);
            chkOnlyPatriarchs.Text = LangMan.LS(LSID.LSID_OnlyPatriarchs);
        }

        private void rgLifeClick(object sender, EventArgs e)
        {
            txtAliveBeforeDate.Enabled = rbAliveBefore.Checked;
        }

        public override void DoReset()
        {
            base.DoReset();
            UpdateSpecific();
        }

        private void UpdateSpecific()
        {
            IndividualListFilter iFilter = (IndividualListFilter)fListMan.Filter;
            GlobalOptions options = GlobalOptions.Instance;

            txtName.Items.Clear();
            txtName.Items.AddRange(GKComboItemSmp.Convert(options.NameFilters.ToArray()));
            txtName.Items.Insert(0, new GKComboItemSmp("*"));

            cmbResidence.Items.Clear();
            cmbResidence.Items.AddRange(GKComboItemSmp.Convert(options.ResidenceFilters.ToArray()));
            cmbResidence.Items.Insert(0, new GKComboItemSmp("*"));

            cmbEventVal.Items.Clear();
            cmbEventVal.Items.AddRange(GKComboItemSmp.Convert(options.EventFilters.ToArray()));

            int lifeSel;
            if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked)
            {
                lifeSel = (int)iFilter.FilterLifeMode;
                rgLife.Enabled = true;
                txtAliveBeforeDate.Text = iFilter.AliveBeforeDate;
            } else {
                lifeSel = -1;
                rgLife.Enabled = false;
                txtAliveBeforeDate.Text = "";
            }

            switch (lifeSel) {
                case 0:
                    rbAll.Checked = true;
                    break;
                case 1:
                    rbOnlyLive.Checked = true;
                    break;
                case 2:
                    rbOnlyDead.Checked = true;
                    break;
                case 3:
                    rbAliveBefore.Checked = true;
                    break;
            }

            int sexSel = (int)iFilter.Sex;
            switch (sexSel) {
                case 0:
                    rbSexAll.Checked = true;
                    break;
                case 1:
                    rbSexMale.Checked = true;
                    break;
                case 2:
                    rbSexFemale.Checked = true;
                    break;
            }

            txtName.Text = iFilter.Name;
            cmbResidence.Text = iFilter.Residence;
            cmbEventVal.Text = iFilter.EventVal;
            chkOnlyPatriarchs.Checked = iFilter.PatriarchOnly;

            GEDCOMTree tree = Base.Context.Tree;

            cmbGroup.Items.Clear();
            //cmbGroup.Sorted = true;
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = tree[i];
                if (rec is GEDCOMGroupRecord) {
                    cmbGroup.Items.Add(new GKComboItem((rec as GEDCOMGroupRecord).GroupName, rec));
                }
            }
            //cmbGroup.Sorted = false;
            cmbGroup.Items.Insert(0, new GKComboItem(LangMan.LS(LSID.LSID_SrcAll), null));
            cmbGroup.Items.Insert(1, new GKComboItem(LangMan.LS(LSID.LSID_SrcNot), null));
            cmbGroup.Items.Insert(2, new GKComboItem(LangMan.LS(LSID.LSID_SrcAny), null));
            if (iFilter.FilterGroupMode != FilterGroupMode.Selected) {
                cmbGroup.SelectedIndex = (int)iFilter.FilterGroupMode;
            } else {
                GEDCOMGroupRecord groupRec = tree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
                if (groupRec != null) cmbGroup.Text = groupRec.GroupName;
            }

            cmbSource.Items.Clear();
            //cmbSource.Sorted = true;
            for (int i = 0; i < tree.RecordsCount; i++) {
                GEDCOMRecord rec = tree[i];
                if (rec is GEDCOMSourceRecord) {
                    cmbSource.Items.Add(new GKComboItem((rec as GEDCOMSourceRecord).FiledByEntry, rec));
                }
            }
            //cmbSource.Sorted = false;
            cmbSource.Items.Insert(0, new GKComboItem(LangMan.LS(LSID.LSID_SrcAll), null));
            cmbSource.Items.Insert(1, new GKComboItem(LangMan.LS(LSID.LSID_SrcNot), null));
            cmbSource.Items.Insert(2, new GKComboItem(LangMan.LS(LSID.LSID_SrcAny), null));
            if (iFilter.SourceMode != FilterGroupMode.Selected) {
                cmbSource.SelectedIndex = (int)iFilter.SourceMode;
            } else {
                GEDCOMSourceRecord sourceRec = tree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
                if (sourceRec != null) cmbSource.Text = sourceRec.FiledByEntry;
            }
        }

        private static void SaveFilter(string flt, StringList filters)
        {
            if (flt != "" && flt != "*" && filters.IndexOf(flt) < 0) filters.Add(flt);
        }

        public override void AcceptChanges()
        {
            base.AcceptChanges();

            IndividualListFilter iFilter = (IndividualListFilter)fListMan.Filter;

            string fs = txtName.Text.Trim();
            SaveFilter(fs, GlobalOptions.Instance.NameFilters);

            fs = cmbResidence.Text.Trim();
            SaveFilter(fs, GlobalOptions.Instance.ResidenceFilters);

            fs = cmbEventVal.Text.Trim();
            SaveFilter(fs, GlobalOptions.Instance.EventFilters);

            iFilter.PatriarchOnly = chkOnlyPatriarchs.Checked.GetValueOrDefault();

            int lifeSel = 0;
            if (rbAll.Checked) lifeSel = 0;
            if (rbOnlyLive.Checked) lifeSel = 1;
            if (rbOnlyDead.Checked) lifeSel = 2;
            if (rbAliveBefore.Checked) lifeSel = 3;

            if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked)
            {
                iFilter.AliveBeforeDate = txtAliveBeforeDate.Text;
                if (lifeSel == 3)
                {
                    try
                    {
                        /*DateTime dt = */
                        DateTime.Parse(txtAliveBeforeDate.Text);
                    }
                    catch
                    {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_DateInvalid));
                        DialogResult = DialogResult.None;
                    }
                }
                iFilter.FilterLifeMode = (FilterLifeMode)lifeSel;
            }

            int sexSel = 0;
            if (rbSexAll.Checked) sexSel = 0;
            if (rbSexMale.Checked) sexSel = 1;
            if (rbSexFemale.Checked) sexSel = 2;
            iFilter.Sex = (GEDCOMSex)sexSel;

            if (txtName.Text == "") txtName.Text = @"*";
            iFilter.Name = txtName.Text;

            if (cmbResidence.Text == "") cmbResidence.Text = @"*";
            iFilter.Residence = cmbResidence.Text;

            if (cmbEventVal.Text == "") cmbEventVal.Text = @"*";
            iFilter.EventVal = cmbEventVal.Text;

            int selectedIndex = cmbGroup.SelectedIndex;
            if (selectedIndex >= 0 && selectedIndex < 3) {
                iFilter.FilterGroupMode = (FilterGroupMode)cmbGroup.SelectedIndex;
                iFilter.GroupRef = "";
            } else {
                GKComboItem item = (GKComboItem)cmbGroup.Items[cmbGroup.SelectedIndex];
                GEDCOMRecord rec = item.Tag as GEDCOMRecord;
                if (rec != null) {
                    iFilter.FilterGroupMode = FilterGroupMode.Selected;
                    iFilter.GroupRef = rec.XRef;
                } else {
                    iFilter.FilterGroupMode = FilterGroupMode.All;
                    iFilter.GroupRef = "";
                }
            }

            selectedIndex = cmbSource.SelectedIndex;
            if (selectedIndex >= 0 && selectedIndex < 3) {
                iFilter.SourceMode = (FilterGroupMode)cmbSource.SelectedIndex;
                iFilter.SourceRef = "";
            } else {
                GKComboItem item = (GKComboItem)cmbSource.Items[cmbSource.SelectedIndex];
                GEDCOMRecord rec = item.Tag as GEDCOMRecord;
                if (rec != null) {
                    iFilter.SourceMode = FilterGroupMode.Selected;
                    iFilter.SourceRef = rec.XRef;
                } else {
                    iFilter.SourceMode = FilterGroupMode.All;
                    iFilter.SourceRef = "";
                }
            }
        }
    }
}
