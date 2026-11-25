/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class RepositoryCitEditDlg : CommonDialog<IRepositoryCitEditDlg, RepositoryCitDlgController>, IRepositoryCitEditDlg
    {
        public GDMRepositoryCitation RepositoryCitation
        {
            get { return fController.RepositoryCitation; }
            set { fController.RepositoryCitation = value; }
        }

        public RepositoryCitEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new RepositoryCitDlgController(this);
            fController.InitView(baseWin);
        }

        private void btnRepositoryAdd_Click(object sender, EventArgs e)
        {
            fController.AddRepository();
        }
    }
}
