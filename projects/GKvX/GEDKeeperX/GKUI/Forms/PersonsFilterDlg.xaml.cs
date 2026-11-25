/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using CommunityToolkit.Maui.Behaviors;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Lists;

namespace GKUI.Forms
{
    public partial class PersonsFilterDlg : CommonDialog<IPersonsFilterDlg, PersonsFilterDlgController>, IPersonsFilterDlg
    {
        private readonly IBaseWindow fBase;
        private readonly IRecordsListModel fListMan;

        public PersonsFilterDlg(IBaseWindow baseWin, IRecordsListModel listMan)
        {
            InitializeComponent();

            var mask = "00/00/0000";
            txtAliveBeforeDate.Behaviors.Add(new MaskedBehavior() { Mask = mask, UnmaskedCharacter = '0' });
            txtAliveBeforeDate.Placeholder = mask;

            if (baseWin == null)
                throw new ArgumentNullException(nameof(baseWin));

            if (listMan == null)
                throw new ArgumentNullException(nameof(listMan));

            fBase = baseWin;
            fListMan = listMan;
            filterView.ListMan = fListMan;

            tabsFilters.SelectedTabIndex = 1;

            fController = new PersonsFilterDlgController(this, listMan);
            fController.InitView(fBase);
            fController.UpdateView();
        }

        private void rgLife_CheckedChanged(object sender, EventArgs e)
        {
            txtAliveBeforeDate.IsEnabled = rbAliveBefore.IsChecked;
        }

        protected async override void AcceptClickHandler(object sender, EventArgs e)
        {
            if (fController.Accept())
                await Close(DialogResult.Ok);
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            fListMan.Filter.Clear();
            fController.UpdateView();
        }
    }
}
