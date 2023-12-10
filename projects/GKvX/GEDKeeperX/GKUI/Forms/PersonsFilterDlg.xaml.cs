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
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using Xamarin.CommunityToolkit.Behaviors;

namespace GKUI.Forms
{
    public partial class PersonsFilterDlg : CommonDialog, ICommonFilterDlg, IPersonsFilterDlg
    {
        private readonly CommonFilterDlgController fCommonController;
        private readonly PersonsFilterDlgController fController;

        private readonly IBaseWindow fBase;
        private readonly IRecordsListModel fListMan;

        #region View Interface

        public IFilterGridView FilterGrid
        {
            get { return filterView; }
        }

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
                    rbAll.IsChecked = true;
                    break;
                case 1:
                    rbOnlyLive.IsChecked = true;
                    break;
                case 2:
                    rbOnlyDead.IsChecked = true;
                    break;
                case 3:
                    rbAliveBefore.IsChecked = true;
                    break;
            }
        }

        void IPersonsFilterDlg.SetSexRadio(int sexSel)
        {
            switch (sexSel) {
                case 0:
                    rbSexAll.IsChecked = true;
                    break;
                case 1:
                    rbSexMale.IsChecked = true;
                    break;
                case 2:
                    rbSexFemale.IsChecked = true;
                    break;
            }
        }

        int IPersonsFilterDlg.GetLifeRadio()
        {
            int lifeSel = 0;
            if (rbAll.IsChecked) lifeSel = 0;
            if (rbOnlyLive.IsChecked) lifeSel = 1;
            if (rbOnlyDead.IsChecked) lifeSel = 2;
            if (rbAliveBefore.IsChecked) lifeSel = 3;
            return lifeSel;
        }

        int IPersonsFilterDlg.GetSexRadio()
        {
            int sexSel = 0;
            if (rbSexAll.IsChecked) sexSel = 0;
            if (rbSexMale.IsChecked) sexSel = 1;
            if (rbSexFemale.IsChecked) sexSel = 2;
            return sexSel;
        }

        void IPersonsFilterDlg.SetLifeEnabled(bool value)
        {
            rgLife.IsEnabled = value;
        }

        #endregion

        public PersonsFilterDlg(IBaseWindow baseWin, IRecordsListModel listMan)
        {
            InitializeComponent();

            var mask = "00/00/0000";
            txtAliveBeforeDate.Behaviors.Add(new MaskedBehavior() { Mask = mask, UnMaskedCharacter = '0' });
            txtAliveBeforeDate.Placeholder = mask;

            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            if (listMan == null)
                throw new ArgumentNullException("listMan");

            fBase = baseWin;
            fListMan = listMan;
            filterView.ListMan = fListMan;

            fCommonController = new CommonFilterDlgController(this, listMan);
            fCommonController.Init(fBase);
            fCommonController.UpdateView();

            tabsFilters.SelectedTabIndex = 1;

            fController = new PersonsFilterDlgController(this, listMan);
            fController.Init(fBase);
            fController.UpdateView();
        }

        private void rgLife_CheckedChanged(object sender, EventArgs e)
        {
            txtAliveBeforeDate.IsEnabled = rbAliveBefore.IsChecked;
        }

        protected async override void AcceptClickHandler(object sender, EventArgs e)
        {
            if (fCommonController.Accept() && fController.Accept())
                await Close(DialogResult.Ok);
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            fListMan.Filter.Clear();
            fCommonController.UpdateView();
            fController.UpdateView();
        }
    }
}
