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
using Terminal.Gui;

namespace GKUI.Forms
{
    using gkITreeView = GKCore.Design.Controls.ITreeView;

    public sealed partial class TTFamilyGroupsDlg : CommonWindow<IFragmentSearchDlg, FragmentSearchController>, IFragmentSearchDlg
    {
        #region View Interface

        gkITreeView IFragmentSearchDlg.GroupsTree
        {
            get { return GetControlHandler<gkITreeView>(tvGroups); }
        }

        ILogChart IFragmentSearchDlg.LogChart
        {
            get { return null; }
        }

        #endregion

        public TTFamilyGroupsDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new FragmentSearchController(this);
            fController.Init(baseWin);
        }

        private void Form_Closed(object sender, Toplevel e)
        {
            fController.SetExternalFilter(null);
        }

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        private void btnAnalyseGroups_Click(object sender, EventArgs e)
        {
            fController.CheckGroups();
        }

        private void tvGroups_DoubleClick(object sender, EventArgs e)
        {
            fController.SelectPerson();
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void miGoToRecord_Click(object sender, EventArgs e)
        {
            fController.SelectPerson();
        }

        private void contextMenu_Opening(object sender, System.ComponentModel.CancelEventArgs e)
        {
            fController.OpeningContextMenu();
        }

        public void miCopyXRef_Click(object sender, EventArgs e)
        {
            fController.CopySelectedXRef();
        }

        #region Data Quality

        private void miResetFilter_Click(object sender, EventArgs e)
        {
            fController.SetExternalFilter(null);
        }

        #endregion
    }
}
