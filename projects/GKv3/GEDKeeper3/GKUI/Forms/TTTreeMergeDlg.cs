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

namespace GKUI.Forms
{
    public sealed partial class TTTreeMergeDlg : CommonDialog<ITreeMergeDlg, TreeMergeController>, ITreeMergeDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnClose;
        private TabPage pageTreeMerge;
        private Label lblMasterBase;
        private TextBox edMasterBase;
        private Label lblOtherBase;
        private TextBox edUpdateBase;
        private Button btnTreeMerge;
        private TextArea mSyncRes;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            fController = new TreeMergeController(this);
            fController.Init(baseWin);
        }

        private void btnTreeMerge_Click(object sender, EventArgs e)
        {
            fController.Merge();
        }
    }
}
