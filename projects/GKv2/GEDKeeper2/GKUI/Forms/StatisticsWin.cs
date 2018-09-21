/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Stats;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class StatisticsWin : CommonForm, IWindow, IStatisticsWin
    {
        private readonly StatisticsWinController fController;

        private readonly ZGraphControl fGraph;
        private readonly GKListView fListStats;

        private StatsMode fCurrentMode;

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

        IComboBoxHandler IStatisticsWin.StatsType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cbType); }
        }

        #endregion

        public StatisticsWin(IBaseWindow baseWin, List<GEDCOMRecord> selectedRecords)
        {
            InitializeComponent();

            tbExcelExport.Image = UIHelper.LoadResourceImage("Resources.btn_excel.gif");

            fGraph = new ZGraphControl();
            fGraph.Dock = DockStyle.Right;
            fGraph.Size = new Size(400, 200);

            Splitter spl = new Splitter();
            spl.Dock = DockStyle.Right;
            spl.Size = new Size(4, 290);
            spl.MinExtra = 100;
            spl.MinSize = 100;
            Panel1.Controls.Add(fGraph);
            Panel1.Controls.Add(spl);

            fListStats = UIHelper.CreateListView(Panel1);
            fListStats.AddColumn("-", 250, false);
            fListStats.AddColumn("-", 150, false);

            Panel1.Controls.SetChildIndex(fListStats, 0);
            Panel1.Controls.SetChildIndex(spl, 2);
            Panel1.Controls.SetChildIndex(fGraph, 3);
            Panel1.Controls.SetChildIndex(ToolBar1, 4);

            fController = new StatisticsWinController(this, selectedRecords);
            fController.Init(baseWin);

            fCurrentMode = StatsMode.smAncestors;

            SetLang();
        }

        private void StatisticsWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        private void cbType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fCurrentMode = (StatsMode)cbType.SelectedIndex;
            fController.CalcStats(fCurrentMode);

            fListStats.SortColumn = -1;
            fListStats.Sorting = SortOrder.None;
        }

        private void StatisticsWin_Load(object sender, EventArgs e)
        {
            fController.UpdateCommonStats();
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MIStats);
            grpSummary.Text = LangMan.LS(LSID.LSID_Summary);

            ColumnHeader1.Text = LangMan.LS(LSID.LSID_Parameter);
            ColumnHeader2.Text = LangMan.LS(LSID.LSID_Total);
            ColumnHeader3.Text = LangMan.LS(LSID.LSID_ManSum);
            ColumnHeader4.Text = LangMan.LS(LSID.LSID_WomanSum);

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
