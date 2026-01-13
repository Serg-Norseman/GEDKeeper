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
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class TTTreeMergeDlg : CommonDialog<ITreeMergeDlg, TreeMergeController>, ITreeMergeDlg
    {
        #region View Interface

        ITextBox ITreeMergeDlg.UpdateBase
        {
            get { return GetControlHandler<ITextBox>(edUpdateBase); }
        }

        ITextBox ITreeMergeDlg.SyncLog
        {
            get { return GetControlHandler<ITextBox>(mSyncRes); }
        }

        #endregion

        public TTTreeMergeDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new TreeMergeController(this);
            fController.Init(baseWin);
        }

        private void btnTreeMerge_Click(object sender, EventArgs e)
        {
            fController.Merge();
        }
    }
}
