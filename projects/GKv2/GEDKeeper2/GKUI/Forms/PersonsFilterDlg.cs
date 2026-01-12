/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;

namespace GKUI.Forms
{
    public partial class PersonsFilterDlg : CommonFilterDlg, IPersonsFilterDlg
    {
        private readonly PersonsFilterDlgController fPersonsFilterDlgController;

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
            get { return GetControlHandler<IComboBox>(cmbEventVal); }
        }

        IComboBox IPersonsFilterDlg.ResidenceCombo
        {
            get { return GetControlHandler<IComboBox>(cmbResidence); }
        }

        IComboBox IPersonsFilterDlg.NameCombo
        {
            get { return GetControlHandler<IComboBox>(txtName); }
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

        public PersonsFilterDlg(IBaseWindow baseWin, IRecordsListModel listMan) : base(baseWin, listMan)
        {
            InitializeComponent();

            tabsFilters.SelectedIndex = 1;

            fPersonsFilterDlgController = new PersonsFilterDlgController(this, listMan);
            fPersonsFilterDlgController.Init(baseWin);
            fPersonsFilterDlgController.UpdateView();
        }

        private void rgLife_CheckedChanged(object sender, EventArgs e)
        {
            txtAliveBeforeDate.Enabled = rbAliveBefore.Checked;
        }

        public override bool Accept()
        {
            return base.Accept() && fPersonsFilterDlgController.Accept();
        }

        public override void Reset()
        {
            base.Reset();
            fPersonsFilterDlgController.UpdateView();
        }

        private void cmbFilter_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Delete && e.Control) {
                var combo = GetControlHandler<IComboBox>(sender);
                fPersonsFilterDlgController.RemoveFilter(combo);
            }
        }
    }
}
