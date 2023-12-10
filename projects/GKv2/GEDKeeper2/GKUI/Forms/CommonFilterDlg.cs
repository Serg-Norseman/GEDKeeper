/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class CommonFilterDlg : CommonDialog<ICommonFilterDlg, CommonFilterDlgController>, ICommonFilterDlg
    {
        private readonly IBaseWindow fBase;
        private readonly IRecordsListModel fListMan;

        private FilterGridView filterView;

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
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
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

            filterView = new FilterGridView(fListMan);
            filterView.Dock = DockStyle.Fill;
            pageFieldsFilter.Controls.Add(filterView);

            fController.UpdateView();
        }

        public virtual bool Accept()
        {
            return fController.Accept();
        }

        public virtual void Reset()
        {
            fController.Reset();
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            Reset();
        }

        protected override void AcceptClickHandler(object sender, EventArgs e)
        {
            try {
                if (Accept())
                    Close(DialogResult.OK);
            } catch (Exception ex) {
                Logger.WriteError("CommonFilterDlg.AcceptClickHandler()", ex);
            }
        }
    }
}
