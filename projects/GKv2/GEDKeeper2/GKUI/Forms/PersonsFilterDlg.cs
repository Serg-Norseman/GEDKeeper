/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP.Controls;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public partial class PersonsFilterDlg : CommonFilterDlg, IPersonsFilterDlg
    {
        private readonly PersonsFilterDlgController fController;

        #region View Interface

        IComboBox IPersonsFilterDlg.SourceCombo
        {
            get { return GetControlHandler<IComboBox>(cmbSource); }
        }

        IComboBox IPersonsFilterDlg.GroupCombo
        {
            get { return GetControlHandler<IComboBox>(cmbGroup); }
        }

        ITextBox IPersonsFilterDlg.AliveBeforeDate
        {
            get { return GetControlHandler<ITextBox>(txtAliveBeforeDate); }
        }

        ICheckBox IPersonsFilterDlg.OnlyPatriarchsCheck
        {
            get { return GetControlHandler<ICheckBox>(chkOnlyPatriarchs); }
        }

        IComboBox IPersonsFilterDlg.EventValCombo
        {
            get { return  GetControlHandler<IComboBox>(cmbEventVal); }
        }

        IComboBox IPersonsFilterDlg.ResidenceCombo
        {
            get { return  GetControlHandler<IComboBox>(cmbResidence); }
        }

        IComboBox IPersonsFilterDlg.NameCombo
        {
            get { return  GetControlHandler<IComboBox>(txtName); }
        }

        void IPersonsFilterDlg.SetLifeRadio(int lifeSel)
        {
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
        }

        void IPersonsFilterDlg.SetSexRadio(int sexSel)
        {
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
        }

        int IPersonsFilterDlg.GetLifeRadio()
        {
            int lifeSel = 0;
            if (rbAll.Checked) lifeSel = 0;
            if (rbOnlyLive.Checked) lifeSel = 1;
            if (rbOnlyDead.Checked) lifeSel = 2;
            if (rbAliveBefore.Checked) lifeSel = 3;
            return lifeSel;
        }

        int IPersonsFilterDlg.GetSexRadio()
        {
            int sexSel = 0;
            if (rbSexAll.Checked) sexSel = 0;
            if (rbSexMale.Checked) sexSel = 1;
            if (rbSexFemale.Checked) sexSel = 2;
            return sexSel;
        }

        void IPersonsFilterDlg.SetLifeEnabled(bool value)
        {
            rgLife.Enabled = value;
        }

        #endregion

        public PersonsFilterDlg()
        {
            InitializeComponent();
        }

        public PersonsFilterDlg(IBaseWindow baseWin, IListManager listMan) : base(baseWin, listMan)
        {
            InitializeComponent();

            // platform: in Mono tsSpecificFilter has 0 index, somehow
            #if __MonoCS__
            tabsFilters.Controls.SetChildIndex(pageSpecificFilter, 1);
            #endif
            tabsFilters.SelectedIndex = 1;

            fController = new PersonsFilterDlgController(this, listMan);
            fController.Init(baseWin);

            SetSpecificLang();
            fController.UpdateView();
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
            lblAliveBefore.Text = LangMan.LS(LSID.LSID_AliveBefore) + ":";
            lblNameMask.Text = LangMan.LS(LSID.LSID_NameMask);
            lblPlaceMask.Text = LangMan.LS(LSID.LSID_PlaceMask);
            lblEventsMask.Text = LangMan.LS(LSID.LSID_EventMask);
            lblGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
            lblSources.Text = LangMan.LS(LSID.LSID_RPSources);
            chkOnlyPatriarchs.Text = LangMan.LS(LSID.LSID_OnlyPatriarchs);
        }

        private void rgLife_CheckedChanged(object sender, EventArgs e)
        {
            txtAliveBeforeDate.Enabled = rbAliveBefore.Checked;
        }

        public override void DoReset()
        {
            base.DoReset();
            fController.UpdateView();
        }

        public override void AcceptChanges()
        {
            base.AcceptChanges();
            fController.Accept();
        }
    }
}
