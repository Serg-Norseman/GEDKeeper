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
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKUI.Controls;

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

        public PersonsFilterDlg(IBaseWindow aBase, ListManager aListMan) : base(aBase, aListMan)
        {
            InitializeComponent();

            this.SetSpecificLang();

            fListMan = (IndividualListMan)aListMan;
            UpdateSpecific();

            // platform: in Mono tsSpecificFilter has 0 index, somehow
            #if GK_LINUX
            tabsFilters.Controls.SetChildIndex(pageSpecificFilter, 1);
            #endif

            tabsFilters.SelectedIndex = 1;
        }

        public void SetSpecificLang()
        {
            this.Text = LangMan.LS(LSID.LSID_MIFilter);
            this.pageSpecificFilter.Text = LangMan.LS(LSID.LSID_PersonsFilter);
            this.rbAll.Text = LangMan.LS(LSID.LSID_All);
            this.rbOnlyLive.Text = LangMan.LS(LSID.LSID_OnlyAlive);
            this.rbOnlyDead.Text = LangMan.LS(LSID.LSID_OnlyDied);
            this.rbAliveBefore.Text = LangMan.LS(LSID.LSID_AliveBefore).ToLower();
            this.rbSexAll.Text = LangMan.LS(LSID.LSID_All);
            this.rbSexMale.Text = LangMan.LS(LSID.LSID_OnlyMans);
            this.rbSexFemale.Text = LangMan.LS(LSID.LSID_OnlyWomans);
            this.lblAliveBefore.Text = LangMan.LS(LSID.LSID_AliveBefore) + ':';
            this.lblNameMask.Text = LangMan.LS(LSID.LSID_NameMask);
            this.lblPlaceMask.Text = LangMan.LS(LSID.LSID_PlaceMask);
            this.lblEventsMask.Text = LangMan.LS(LSID.LSID_EventMask);
            this.lblGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
            this.lblSources.Text = LangMan.LS(LSID.LSID_RPSources);
            this.chkOnlyPatriarchs.Text = LangMan.LS(LSID.LSID_OnlyPatriarchs);
        }

        private void rgLifeClick(object sender, EventArgs e)
        {
            this.txtAliveBeforeDate.Enabled = this.rbAliveBefore.Checked;
        }

        public override void DoReset()
        {
            base.DoReset();
            this.UpdateSpecific();
        }
        
        private void UpdateSpecific()
        {
            IndividualListFilter iFilter = (IndividualListFilter)fListMan.Filter;
            GlobalOptions options = MainWin.Instance.Options;
            
            this.txtName.Items.Clear();
            this.txtName.Items.AddRange(options.NameFilters.ToArray());
            this.txtName.Items.Insert(0, "*");

            this.cmbResidence.Items.Clear();
            this.cmbResidence.Items.AddRange(options.ResidenceFilters.ToArray());
            this.cmbResidence.Items.Insert(0, "*");

            this.cmbEventVal.Items.Clear();
            this.cmbEventVal.Items.AddRange(options.EventFilters.ToArray());

            int lifeSel;
            if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked)
            {
                lifeSel = (int)iFilter.FilterLifeMode;
                this.rgLife.Enabled = true;
                this.txtAliveBeforeDate.Text = iFilter.AliveBeforeDate;
            } else {
                lifeSel = -1;
                this.rgLife.Enabled = false;
                this.txtAliveBeforeDate.Text = "";
            }

            switch (lifeSel) {
                case 0:
                    this.rbAll.Checked = true;
                    break;
                case 1:
                    this.rbOnlyLive.Checked = true;
                    break;
                case 2:
                    this.rbOnlyDead.Checked = true;
                    break;
                case 3:
                    this.rbAliveBefore.Checked = true;
                    break;
            }

            int sexSel = (int)iFilter.Sex;
            switch (sexSel) {
                case 0:
                    this.rbSexAll.Checked = true;
                    break;
                case 1:
                    this.rbSexMale.Checked = true;
                    break;
                case 2:
                    this.rbSexFemale.Checked = true;
                    break;
            }

            this.txtName.Text = iFilter.Name;
            this.cmbResidence.Text = iFilter.Residence;
            this.cmbEventVal.Text = iFilter.EventVal;
            this.chkOnlyPatriarchs.Checked = iFilter.PatriarchOnly;

            GEDCOMTree tree = this.Base.Tree;

            this.cmbGroup.Items.Clear();
            this.cmbGroup.Sorted = true;
            int num = tree.RecordsCount - 1;
            for (int i = 0; i <= num; i++) {
                GEDCOMRecord rec = tree[i];
                if (rec is GEDCOMGroupRecord) {
                    this.cmbGroup.Items.Add(new GKComboItem((rec as GEDCOMGroupRecord).GroupName, rec));
                }
            }
            this.cmbGroup.Sorted = false;
            this.cmbGroup.Items.Insert(0, new GKComboItem(LangMan.LS(LSID.LSID_SrcAll), null));
            this.cmbGroup.Items.Insert(1, new GKComboItem(LangMan.LS(LSID.LSID_SrcNot), null));
            this.cmbGroup.Items.Insert(2, new GKComboItem(LangMan.LS(LSID.LSID_SrcAny), null));
            if (iFilter.FilterGroupMode != FilterGroupMode.Selected) {
                this.cmbGroup.SelectedIndex = (int)iFilter.FilterGroupMode;
            } else {
                GEDCOMGroupRecord groupRec = tree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
                this.cmbGroup.Text = groupRec.GroupName;
            }

            this.cmbSource.Items.Clear();
            this.cmbSource.Sorted = true;
            for (int i = 0; i <= tree.RecordsCount - 1; i++) {
                GEDCOMRecord rec = tree[i];
                if (rec is GEDCOMSourceRecord) {
                    this.cmbSource.Items.Add(new GKComboItem((rec as GEDCOMSourceRecord).FiledByEntry, rec));
                }
            }
            this.cmbSource.Sorted = false;
            this.cmbSource.Items.Insert(0, new GKComboItem(LangMan.LS(LSID.LSID_SrcAll), null));
            this.cmbSource.Items.Insert(1, new GKComboItem(LangMan.LS(LSID.LSID_SrcNot), null));
            this.cmbSource.Items.Insert(2, new GKComboItem(LangMan.LS(LSID.LSID_SrcAny), null));
            if (iFilter.SourceMode != FilterGroupMode.Selected) {
                this.cmbSource.SelectedIndex = (int)iFilter.SourceMode;
            } else {
                GEDCOMSourceRecord sourceRec = tree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
                this.cmbSource.Text = sourceRec.FiledByEntry;
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
            
            string fs = this.txtName.Text.Trim();
            SaveFilter(fs, MainWin.Instance.Options.NameFilters);

            fs = this.cmbResidence.Text.Trim();
            SaveFilter(fs, MainWin.Instance.Options.ResidenceFilters);

            fs = this.cmbEventVal.Text.Trim();
            SaveFilter(fs, MainWin.Instance.Options.EventFilters);

            iFilter.PatriarchOnly = this.chkOnlyPatriarchs.Checked;

            int lifeSel = 0;
            if (this.rbAll.Checked) lifeSel = 0;
            if (this.rbOnlyLive.Checked) lifeSel = 1;
            if (this.rbOnlyDead.Checked) lifeSel = 2;
            if (this.rbAliveBefore.Checked) lifeSel = 3;

            if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked)
            {
                iFilter.AliveBeforeDate = this.txtAliveBeforeDate.Text;
                if (lifeSel == 3)
                {
                    try
                    {
                        /*DateTime dt = */
                        DateTime.Parse(this.txtAliveBeforeDate.Text);
                    }
                    catch
                    {
                        GKUtils.ShowError(LangMan.LS(LSID.LSID_DateInvalid));
                        base.DialogResult = DialogResult.None;
                    }
                }
                iFilter.FilterLifeMode = (FilterLifeMode)lifeSel;
            }

            int sexSel = 0;
            if (this.rbSexAll.Checked) sexSel = 0;
            if (this.rbSexMale.Checked) sexSel = 1;
            if (this.rbSexFemale.Checked) sexSel = 2;
            iFilter.Sex = (GEDCOMSex)sexSel;

            if (this.txtName.Text == "") this.txtName.Text = @"*";
            iFilter.Name = this.txtName.Text;

            if (this.cmbResidence.Text == "") this.cmbResidence.Text = @"*";
            iFilter.Residence = this.cmbResidence.Text;

            if (this.cmbEventVal.Text == "") this.cmbEventVal.Text = @"*";
            iFilter.EventVal = this.cmbEventVal.Text;

            int selectedIndex = this.cmbGroup.SelectedIndex;
            if (selectedIndex >= 0 && selectedIndex < 3) {
                iFilter.FilterGroupMode = (FilterGroupMode)this.cmbGroup.SelectedIndex;
                iFilter.GroupRef = "";
            } else {
                GKComboItem item = (GKComboItem)this.cmbGroup.Items[this.cmbGroup.SelectedIndex];
                GEDCOMRecord rec = item.Tag as GEDCOMRecord;
                if (rec != null) {
                    iFilter.FilterGroupMode = FilterGroupMode.Selected;
                    iFilter.GroupRef = rec.XRef;
                } else {
                    iFilter.FilterGroupMode = FilterGroupMode.All;
                    iFilter.GroupRef = "";
                }
            }

            int selectedIndex2 = this.cmbSource.SelectedIndex;
            if (selectedIndex2 >= 0 && selectedIndex2 < 3) {
                iFilter.SourceMode = (FilterGroupMode)this.cmbSource.SelectedIndex;
                iFilter.SourceRef = "";
            } else {
                GKComboItem item = (GKComboItem)this.cmbSource.Items[this.cmbSource.SelectedIndex];
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
