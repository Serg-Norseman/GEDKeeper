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
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTTreeCheckDlg : CommonDialog<ITreeCheckDlg, TreeCheckController>, ITreeCheckDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnClose;
        private TabPage pageTreeCheck;
        private Button btnAnalyseBase;
        private Button btnBaseRepair;
        private ButtonMenuItem miDetails;
        private ButtonMenuItem miGoToRecord;
        private ButtonMenuItem miCopyXRef;
        private GKListView ListChecks;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        #region View Interface

        IListView ITreeCheckDlg.ChecksList
        {
            get { return ListChecks; }
        }

        #endregion

        public TTTreeCheckDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

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
