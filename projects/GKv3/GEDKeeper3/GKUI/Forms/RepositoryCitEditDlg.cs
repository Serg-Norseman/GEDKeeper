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
using Eto.Forms;
using Eto.Serialization.Xaml;
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
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabControl tabsData;
        private GroupBox GroupBox1;
        private ComboBox cmbRepository;
        private Label lblRepository;
        private TabPage pageCallNumbers;
        private Button btnAccept;
        private Button btnCancel;
        private Button btnRepositoryAdd;
        private GKSheetList fCallNumbersList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            fController = new RepositoryCitDlgController(this);
            fController.Init(baseWin);
        }

        private void btnRepositoryAdd_Click(object sender, EventArgs e)
        {
            fController.AddRepository();
        }

        private void cmbRepository_KeyUp(object sender, KeyEventArgs e)
        {
            var text = cmbRepository.Text;
            fController.RefreshRepositoriesList(cmbRepository.Text);
            cmbRepository.Text = text;
            //cmbRepository.SelectionStart = cmbRepository.Text.Length;
        }
    }
}
