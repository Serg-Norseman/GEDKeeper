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
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class CommonFilterDlg : CommonDialog<ICommonFilterDlg, CommonFilterDlgController>, ICommonFilterDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabControl tabsFilters;
        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageFieldsFilter;
        private TabPage pageSpecificFilter;
        private Button btnReset;
        private FilterGridView filterView;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly IBaseWindow fBase;
        private readonly IRecordsListModel fListMan;

        public IBaseWindow Base
        {
            get { return fBase; }
        }

        public IRecordsListModel ListMan
        {
            get { return fListMan; }
        }

        #region View Interface

        public IFilterGridView FilterGrid
        {
            get { return filterView; }
        }

        #endregion

        public CommonFilterDlg()
        {
            XamlReader.Load(this);
        }

        public CommonFilterDlg(IBaseWindow baseWin, IRecordsListModel listMan) : this()
        {
            if (baseWin == null)
                throw new ArgumentNullException(nameof(baseWin));

            if (listMan == null)
                throw new ArgumentNullException(nameof(listMan));

            fBase = baseWin;
            fListMan = listMan;

            fController = new CommonFilterDlgController(this, listMan);
            fController.Init(baseWin);

            filterView.ListMan = fListMan;

            fController.UpdateView();
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            fController.Reset();
        }
    }
}
