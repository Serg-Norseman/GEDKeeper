/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class StatisticsWin : CommonWindow, IStatisticsWin
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private GroupBox grpSummary;
        private ComboBox cbType;
        private GKListView lvSummary;
        private Button tbExcelExport;
        //private ContextMenu cmStatTypes;
        private ZGraphControl fGraph;
        private GKListView fListStats;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            fController = new StatisticsWinController(this, selectedRecords);
            fController.Init(baseWin);
        }

        private void StatisticsWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
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
