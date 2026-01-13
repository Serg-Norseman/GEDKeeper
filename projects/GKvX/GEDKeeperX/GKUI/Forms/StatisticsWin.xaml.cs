/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class StatisticsWin : CommonWindow, IStatisticsWin
    {
        private readonly StatisticsWinController fController;

        #region View Interface

        IGraphControl IStatisticsWin.Graph
        {
            get { return fGraph; }
        }

        IListView IStatisticsWin.ListStats
        {
            get { return fListStats; }
        }

        IListView IStatisticsWin.Summary
        {
            get { return lvSummary; }
        }

        IComboBox IStatisticsWin.StatsType
        {
            get { return GetControlHandler<IComboBox>(cbType); }
        }

        #endregion

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }


        public StatisticsWin(IBaseWindow baseWin, List<GDMRecord> selectedRecords)
        {
            InitializeComponent();

            fController = new StatisticsWinController(this, selectedRecords);
            fController.Init(baseWin);
        }

        private void cbType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.CalcStats();
        }

        private void StatisticsWin_Load(object sender, EventArgs e)
        {
            fController.UpdateCommonStats();
        }

        public override void SetLocale()
        {
            fController.SetLocale();

            fController.UpdateCommonStats();
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        private void tbExcelExport_Click(object sender, EventArgs e)
        {
            fController.ExportToExcel();
        }
    }
}
