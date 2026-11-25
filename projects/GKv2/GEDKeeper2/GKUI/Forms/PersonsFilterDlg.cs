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

        public PersonsFilterDlg()
        {
            InitializeComponent();
        }

        public PersonsFilterDlg(IBaseWindow baseWin, IRecordsListModel listMan) : base(baseWin, listMan)
        {
            InitializeComponent();

            tabsFilters.SelectedIndex = 1;

            fPersonsFilterDlgController = new PersonsFilterDlgController(this, listMan);
            fPersonsFilterDlgController.InitView(baseWin);
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
                var combo = fController.GetControlHandler<IComboBox>(sender);
                fPersonsFilterDlgController.RemoveFilter(combo);
            }
        }
    }
}
