/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
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
        private readonly ZGraphControl fGraph;
        private readonly GKListView fListStats;

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

            Panel1.Controls.SetChildIndex(fListStats, 0);
            Panel1.Controls.SetChildIndex(spl, 2);
            Panel1.Controls.SetChildIndex(fGraph, 3);
            Panel1.Controls.SetChildIndex(ToolBar1, 4);

            fController = new StatisticsWinController(this, selectedRecords);
            fController.Init(baseWin);
        }

        private void StatisticsWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
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
