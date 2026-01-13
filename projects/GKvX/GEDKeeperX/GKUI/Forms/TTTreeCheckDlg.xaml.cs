/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class TTTreeCheckDlg : CommonDialog<ITreeCheckDlg, TreeCheckController>, ITreeCheckDlg
    {
        #region View Interface

        IListView ITreeCheckDlg.ChecksList
        {
            get { return ListChecks; }
        }

        #endregion

        public TTTreeCheckDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new TreeCheckController(this);
            fController.Init(baseWin);
        }

        /*
        <comcom:GKListView.ContextMenu>
            <ContextMenu Opening="contextMenu_Opening">
                <ButtonMenuItem x:Name="miDetails" Click="miDetails_Click" />
                <ButtonMenuItem x:Name="miGoToRecord" Click="miGoToRecord_Click" />
                <ButtonMenuItem x:Name="miCopyXRef" Click="miCopyXRef_Click" />
            </ContextMenu>
        </comcom:GKListView.ContextMenu>
         */

        private void btnAnalyseBase_Click(object sender, EventArgs e)
        {
            fController.CheckBase();
        }

        private async void btnBaseRepair_Click(object sender, EventArgs e)
        {
            await fController.Repair();
        }

        private void ListChecks_DblClick(object sender, EventArgs e)
        {
            fController.SelectRecord();
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void miGoToRecord_Click(object sender, EventArgs e)
        {
            fController.SelectRecord();
        }

        private void contextMenu_Opening(object sender, EventArgs e)
        {
            fController.OpeningContextMenu();
        }

        public void miCopyXRef_Click(object sender, EventArgs e)
        {
            fController.CopySelectedXRefs();
        }
    }
}
