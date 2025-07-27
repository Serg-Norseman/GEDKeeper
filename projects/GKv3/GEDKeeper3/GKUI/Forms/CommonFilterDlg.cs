/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
                throw new ArgumentNullException("baseWin");

            if (listMan == null)
                throw new ArgumentNullException("listMan");

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
