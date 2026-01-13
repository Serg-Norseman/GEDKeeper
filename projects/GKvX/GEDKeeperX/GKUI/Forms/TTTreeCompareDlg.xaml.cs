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
    public sealed partial class TTTreeCompareDlg : CommonDialog<ITreeCompareDlg, TreeCompareController>, ITreeCompareDlg
    {
        #region View Interface

        ITextBox ITreeCompareDlg.ExternalBase
        {
            get { return GetControlHandler<ITextBox>(txtCompareFile); }
        }

        ITextBox ITreeCompareDlg.CompareOutput
        {
            get { return GetControlHandler<ITextBox>(ListCompare); }
        }

        #endregion

        public TTTreeCompareDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new TreeCompareController(this);
            fController.Init(baseWin);
        }

        private void btnFileChoose_Click(object sender, EventArgs e)
        {
            fController.SelectExternalFile();
        }

        private void btnMatch_Click(object sender, EventArgs e)
        {
            fController.Match();
        }

        private void rbtnMatch_CheckedChanged(object sender, EventArgs e)
        {
            // prevent triggering on incomplete initialization
            if (fController != null) {
                fController.ChangeTreeMatchType();
            }
        }
    }
}
