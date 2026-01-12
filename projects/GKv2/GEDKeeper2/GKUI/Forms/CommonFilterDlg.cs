/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GKCore;
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

        private FilterGridView filterView;

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
            InitializeComponent();
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
