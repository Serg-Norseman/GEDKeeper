/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design;
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Stats;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class StatisticsWin : CommonWindow, IStatisticsWin
    {
        #region Design components

        private GroupBox grpSummary;
        private Panel panDataPlaceholder;
        private /*ToolBar*/Panel ToolBar1;
        private /*ButtonToolItem*/ComboBox cbType;
        private GKListView lvSummary;
        private /*ButtonToolItem*/Button tbExcelExport;
        //private ContextMenu cmStatTypes;

        #endregion

        private readonly StatisticsWinController fController;

        private readonly ZGraphControl fGraph;
        private readonly GKListView fListStats;

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

            tbExcelExport.Image = UIHelper.LoadResourceImage("Resources.btn_excel.gif");

            fGraph = new ZGraphControl();

            fListStats = new GKListView();
            fListStats.AddColumn("-", 250, false);
            fListStats.AddColumn("-", 150, false);

            Splitter spl = new Splitter();
            spl.Panel1 = fListStats;
            spl.Panel2 = fGraph;
            spl.RelativePosition = 500;
            spl.Orientation = Orientation.Horizontal;
            spl.FixedPanel = SplitterFixedPanel.Panel1;
            panDataPlaceholder.Content = spl;

            fController = new StatisticsWinController(this, selectedRecords);
            fController.Init(baseWin);

            SetLocale();
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
            Title = LangMan.LS(LSID.LSID_MIStats);
            grpSummary.Text = LangMan.LS(LSID.LSID_Summary);

            lvSummary.ClearColumns();
            lvSummary.AddColumn(LangMan.LS(LSID.LSID_Parameter), 300);
            lvSummary.AddColumn(LangMan.LS(LSID.LSID_Total), 100);
            lvSummary.AddColumn(LangMan.LS(LSID.LSID_ManSum), 100);
            lvSummary.AddColumn(LangMan.LS(LSID.LSID_WomanSum), 100);

            SetToolTip(tbExcelExport, LangMan.LS(LSID.LSID_MIExportToExcelFile));
            fController.UpdateCommonStats();

            int oldIndex = cbType.SelectedIndex;
            fController.UpdateStatsTypes();
            cbType.SelectedIndex = oldIndex;
        }

        private void tbExcelExport_Click(object sender, EventArgs e)
        {
            fController.ExportToExcel();
        }
    }
}
