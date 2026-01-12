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
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TreeFilterDlg : CommonDialog<ITreeFilterDlg, TreeFilterDlgController>, ITreeFilterDlg
    {
        private readonly GKSheetList fPersonsList;

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

            fPersonsList = new GKSheetList(Panel1);

            fController = new TreeFilterDlgController(this);
            fController.Init(baseWin);
        }

        private void rbCutNoneClick(object sender, EventArgs e)
        {
            fController.ChangeCutMode();
        }

        private void TreeFilterDlg_Load(object sender, EventArgs e)
        {
            fController.UpdateView();
        }
    }
}
