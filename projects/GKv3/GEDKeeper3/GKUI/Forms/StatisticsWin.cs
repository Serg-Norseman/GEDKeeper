/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
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

        public StatisticsWin(IBaseWindow baseWin, List<GDMRecord> selectedRecords)
        {
            XamlReader.Load(this);

            fListStats.AddColumn("-", 250, false);
            fListStats.AddColumn("-", 150, false);

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

            fListStats.SortOrder = BSDTypes.SortOrder.None;
            fListStats.SortColumn = -1;
            fListStats.Sorting = true;
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

        private void tbExcelExport_Click(object sender, EventArgs e)
        {
            fController.ExportToExcel();
        }
    }
}
