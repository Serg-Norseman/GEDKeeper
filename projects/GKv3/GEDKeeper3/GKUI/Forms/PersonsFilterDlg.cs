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
    public partial class PersonsFilterDlg : CommonDialog<IPersonsFilterDlg, PersonsFilterDlgController>, IPersonsFilterDlg
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
        private Label lblSources;
        private CheckBox chkOnlyPatriarchs;
        private Label lblGroups;
        private Label lblPlaceMask;
        private Label lblNameMask;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly IBaseWindow fBase;
        private readonly IRecordsListModel fListMan;

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

            tabsFilters.SelectedIndex = 1;

            fController = new PersonsFilterDlgController(this, listMan);
            fController.InitView(baseWin);
            fController.UpdateView();
        }

        private void rgLife_CheckedChanged(object sender, EventArgs e)
        {
            txtAliveBeforeDate.Enabled = rbAliveBefore.Checked;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            if (fController.Accept())
                Close(DialogResult.Ok);
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            fListMan.Filter.Clear();
            fController.UpdateView();
        }

        private void cmbFilter_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Delete && e.Control) {
                var combo = fController.GetControlHandler<IComboBox>(sender);
                fController.RemoveFilter(combo);
            }
        }
    }
}
