/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class RepositoryCitEditDlg : CommonDialog<IRepositoryCitEditDlg, RepositoryCitDlgController>, IRepositoryCitEditDlg
    {
        private readonly GKSheetList fCallNumbersList;

        public GDMRepositoryCitation RepositoryCitation
        {
            get { return fController.RepositoryCitation; }
            set { fController.RepositoryCitation = value; }
        }

        #region View Interface

        IComboBox IRepositoryCitEditDlg.RepositoryCombo
        {
            get { return GetControlHandler<IComboBox>(cmbRepository); }
        }

        ISheetList IRepositoryCitEditDlg.CallNumbersList
        {
            get { return fCallNumbersList; }
        }

        #endregion

        public RepositoryCitEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fCallNumbersList = new GKSheetList(pageCallNumbers);

            fController = new RepositoryCitDlgController(this);
            fController.Init(baseWin);
        }

        private void btnRepositoryAdd_Click(object sender, EventArgs e)
        {
            fController.AddRepository();
        }

        private void cmbRepository_KeyUp(object sender, KeyEventArgs e)
        {
            fController.RefreshRepositoriesList(cmbRepository.Text);
            cmbRepository.SelectionStart = cmbRepository.Text.Length;
        }
    }
}
