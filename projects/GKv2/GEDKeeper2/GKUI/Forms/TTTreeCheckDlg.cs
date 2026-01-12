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
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTTreeCheckDlg : CommonDialog<ITreeCheckDlg, TreeCheckController>, ITreeCheckDlg
    {
        private GKListView ListChecks;

        #region View Interface

        IListView ITreeCheckDlg.ChecksList
        {
            get { return ListChecks; }
        }

        #endregion

        public TTTreeCheckDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            ListChecks = UIHelper.CreateListView(panProblemsContainer);
            ListChecks.DoubleClick += ListChecks_DblClick;
            ListChecks.CheckBoxes = true;
            ListChecks.ContextMenuStrip = contextMenu;

            fController = new TreeCheckController(this);
            fController.Init(baseWin);
        }

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

        private void contextMenu_Opening(object sender, System.ComponentModel.CancelEventArgs e)
        {
            fController.OpeningContextMenu();
        }

        public void miCopyXRef_Click(object sender, EventArgs e)
        {
            fController.CopySelectedXRefs();
        }
    }
}
