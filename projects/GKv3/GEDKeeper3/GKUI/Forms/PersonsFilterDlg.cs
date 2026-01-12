/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class PersonsFilterDlg : CommonDialog, ICommonFilterDlg, IPersonsFilterDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabControl tabsFilters;
        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageFieldsFilter;
        private TabPage pageSpecificFilter;
        private Button btnReset;
        private FilterGridView filterView;
        private ComboBox cmbEventVal;
        private ComboBox cmbSource;
        private ComboBox cmbGroup;
        private ComboBox cmbResidence;
        private MaskedTextBox txtAliveBeforeDate;
        private RadioButton rbSexMale;
        private RadioButton rbSexAll;
        private RadioButton rbSexFemale;
        private GroupBox rgSex;
        private ComboBox txtName;
        private RadioButton rbAll;
        private RadioButton rbOnlyLive;
        private RadioButton rbOnlyDead;
        private RadioButton rbAliveBefore;
        private GroupBox rgLife;
        private Label lblEventsMask;
        private Label lblAliveBefore;
        private Label lblSources;
        private CheckBox chkOnlyPatriarchs;
        private Label lblGroups;
        private Label lblPlaceMask;
        private Label lblNameMask;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            UIHelper.FixRadioButtons(this, rgSex);
            UIHelper.FixRadioButtons(this, rgLife);

            txtAliveBeforeDate.Provider = new FixedMaskedTextProvider("00/00/0000");
        }

        public PersonsFilterDlg(IBaseWindow baseWin, IRecordsListModel listMan) : this()
        {
            if (baseWin == null)
                throw new ArgumentNullException(nameof(baseWin));

            if (listMan == null)
                throw new ArgumentNullException(nameof(listMan));

            fBase = baseWin;
            fListMan = listMan;
            filterView.ListMan = fListMan;

            fCommonController = new CommonFilterDlgController(this, listMan);
            fCommonController.Init(baseWin);
            fCommonController.UpdateView();

            tabsFilters.SelectedIndex = 1;

            fController = new PersonsFilterDlgController(this, listMan);
            fController.Init(baseWin);
            fController.UpdateView();
        }

        private void rgLife_CheckedChanged(object sender, EventArgs e)
        {
            txtAliveBeforeDate.Enabled = rbAliveBefore.Checked;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            if (fCommonController.Accept() && fController.Accept())
                Close(DialogResult.Ok);
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            fListMan.Filter.Clear();
            fCommonController.UpdateView();
            fController.UpdateView();
        }

        private void cmbFilter_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Delete && e.Control) {
                var combo = GetControlHandler<IComboBox>(sender);
                fController.RemoveFilter(combo);
            }
        }
    }
}
