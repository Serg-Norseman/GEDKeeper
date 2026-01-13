/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;

namespace GKUI.Forms
{
    public sealed partial class TreeFilterDlg : CommonDialog<ITreeFilterDlg, TreeFilterDlgController>, ITreeFilterDlg
    {
        public ChartFilter Filter
        {
            get { return fController.Filter; }
            set { fController.Filter = value; }
        }

        #region View Interface

        ISheetList ITreeFilterDlg.PersonsList
        {
            get { return fPersonsList; }
        }

        INumericBox ITreeFilterDlg.YearNum
        {
            get { return GetControlHandler<INumericBox>(edYear); }
        }

        IComboBox ITreeFilterDlg.SourceCombo
        {
            get { return GetControlHandler<IComboBox>(cmbSource); }
        }

        #endregion

        public TreeFilterDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new TreeFilterDlgController(this);
            fController.Init(baseWin);
        }

        private void rbCutX_CheckedChanged(object sender, EventArgs e)
        {
            // prevent triggering on incomplete initialization
            if (fController != null) {
                fController.ChangeCutMode();
            }
        }

        private void TreeFilterDlg_Load(object sender, EventArgs e)
        {
            fController.UpdateView();
        }
    }
}
