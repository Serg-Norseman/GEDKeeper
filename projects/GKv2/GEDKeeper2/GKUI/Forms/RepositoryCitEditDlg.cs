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
using System.Windows.Forms;
using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
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

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnRepositoryAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");

            fCallNumbersList = new GKSheetList(pageCallNumbers);
            fCallNumbersList.SetControlName("fCallNumbersList"); // for purpose of tests

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
