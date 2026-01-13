/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public partial class FindAndReplaceDlg : CommonWindow<IFARDlg, FARDlgController>, IFARDlg
    {
        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }

        public FindAndReplaceDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new FARDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }

        private void btnPrev_Click(object sender, EventArgs e)
        {
            fController.Prev();
        }

        private void btnNext_Click(object sender, EventArgs e)
        {
            fController.Next();
        }

        private void btnReplace_Click(object sender, EventArgs e)
        {
            fController.Replace();
        }

        private void btnReplaceAll_Click(object sender, EventArgs e)
        {
            fController.ReplaceAll();
        }
    }
}
