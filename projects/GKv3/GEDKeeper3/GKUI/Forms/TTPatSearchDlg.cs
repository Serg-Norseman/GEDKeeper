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
    public sealed partial class TTPatSearchDlg : CommonDialog<IPatriarchsSearchDlg, PatriarchsSearchController>, IPatriarchsSearchDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnClose;
        private TabPage pagePatSearch;
        private Button btnPatSearch;
        private Panel panPatriarchsContainer;
        private Label lblMinGenerations;
        private NumericStepper edMinGens;
        private Button btnSetPatriarch;
        private Button btnPatriarchsDiagram;
        private CheckBox chkWithoutDates;
        private GKListView ListPatriarchs;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        #region View Interface

        INumericBox IPatriarchsSearchDlg.MinGensNum
        {
            get { return GetControlHandler<INumericBox>(edMinGens); }
        }

        ICheckBox IPatriarchsSearchDlg.WithoutDatesCheck
        {
            get { return GetControlHandler<ICheckBox>(chkWithoutDates); }
        }

        IListView IPatriarchsSearchDlg.PatriarchsList
        {
            get { return ListPatriarchs; }
        }

        #endregion

        public TTPatSearchDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new PatriarchsSearchController(this);
            fController.Init(baseWin);
        }

        private void ListPatriarchs_DblClick(object sender, EventArgs e)
        {
            fController.SelectPatriarch();
        }

        private void btnPatSearch_Click(object sender, EventArgs e)
        {
            fController.Search();
        }

        private void btnSetPatriarch_Click(object sender, EventArgs e)
        {
            fController.SetPatriarch();
        }

        private void btnPatriarchsDiagram_Click(object sender, EventArgs e)
        {
            fController.ShowPatriarchsDiagram();
        }
    }
}
