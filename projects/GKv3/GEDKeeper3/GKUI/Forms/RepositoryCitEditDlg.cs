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
