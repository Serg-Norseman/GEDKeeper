/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

            ListChecks.AddCheckedColumn(@"x", 50, false);

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

        private void btnBaseRepair_Click(object sender, EventArgs e)
        {
            fController.Repair();
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
            var list = ListChecks.GetSelectedItems();
            fController.CopySelectedXRefs(list);
        }
    }
}
