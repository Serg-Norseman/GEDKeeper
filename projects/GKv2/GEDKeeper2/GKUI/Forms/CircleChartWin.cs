/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class CircleChartWin : PrintableForm, ICircleChartWin
    {
        private readonly CircleChartWinController fController;

        private readonly CircleChart fCircleChart;

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }

        #region View Interface

        ICircleChart ICircleChartWin.CircleChart
        {
            get { return fCircleChart; }
        }

        #endregion

        public CircleChartWin(IBaseWindow baseWin, GDMIndividualRecord startPerson, CircleChartType type)
        {
            InitializeComponent();

            tbImageSave.Image = UIHelper.LoadResourceImage("Resources.btn_save_image.gif");
            tbDocPreview.Image = UIHelper.LoadResourceImage("Resources.btn_preview.gif");
            tbDocPrint.Image = UIHelper.LoadResourceImage("Resources.btn_print.gif");
            tbPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");
            tbNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");
            tbOptions.Image = UIHelper.LoadResourceImage("Resources.btn_tools.gif");

            ToolBar1.Visible = true;

            UIHelper.FixToolStrip(ToolBar1);

            fCircleChart = new CircleChart();
            fCircleChart.Base = baseWin;
            fCircleChart.ChartType = type;
            fCircleChart.NavRefresh += CircleChartWin_NavRefresh;
            fCircleChart.ZoomChanged += CircleChartWin_NavRefresh;
            fCircleChart.RootChanged += CircleChartWin_RootChanged;
            fCircleChart.RootPerson = startPerson;
            fCircleChart.Options.Assign(GlobalOptions.Instance.CircleChartOptions);
            fCircleChart.Name = "fCircleChart";
            fCircleChart.Dock = DockStyle.Fill;
            Controls.Add(fCircleChart);
            Controls.SetChildIndex(fCircleChart, 0);

            fController = new CircleChartWinController(this);
            fController.Init(baseWin);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            AppHost.Instance.SetWindowBounds(this, GlobalOptions.Instance.ChartWindowsShowMode);

            fCircleChart.Select();
            UpdateControls();
        }

        protected override void OnClosed(EventArgs e)
        {
            AppHost.Instance.CloseDependentWindows(this);
            base.OnClosed(e);
        }

        protected override IPrintable GetPrintable()
        {
            return fCircleChart;
        }

        private void CircleChartWin_NavRefresh(object sender, EventArgs e)
        {
            GenChart();
        }

        private void CircleChartWin_RootChanged(object sender, GDMIndividualRecord person)
        {
            GenChart();
        }

        private void CircleChartWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        private void ToolBar1_ButtonClick(object sender, EventArgs e)
        {
            if (sender == tbPrev) {
                NavPrev();
            } else if (sender == tbNext) {
                NavNext();
            }
        }

        private void tbImageSave_Click(object sender, EventArgs e)
        {
            fController.SaveSnapshot();
        }

        private void tbDocPreview_Click(object sender, EventArgs e)
        {
            DoPrintPreview();
        }

        private void tbDocPrint_Click(object sender, EventArgs e)
        {
            DoPrint();
        }

        private void tbOptions_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowOptions(this, OptionsPage.opCircleChart);
        }

        private void miFanMode_Click(object sender, EventArgs e)
        {
            miFanMode.Checked = !miFanMode.Checked;

            fCircleChart.Model.FanMode = miFanMode.Checked;
            fCircleChart.Changed();
        }

        #region ILocalizable implementation

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        #endregion

        #region IChartWindow implementation

        public void GenChart()
        {
            UpdateControls();
        }

        #endregion

        #region IWorkWindow implementation

        public void UpdateControls()
        {
            try {
                StatusLines[0] = string.Format(LangMan.LS(LSID.TreeIndividualsCount), fCircleChart.Model.IndividualsCount);
                var imageSize = fCircleChart.GetImageSize();
                StatusLines[1] = string.Format(LangMan.LS(LSID.ImageSize), imageSize.Width, imageSize.Height);

                tbPrev.Enabled = NavCanBackward();
                tbNext.Enabled = NavCanForward();

                AppHost.Instance.UpdateControls(false, true);
            } catch (Exception ex) {
                Logger.WriteError("CircleChartWin.UpdateControls()", ex);
            }
        }

        public void UpdateSettings()
        {
            fCircleChart.Options.Assign(GlobalOptions.Instance.CircleChartOptions);
            fCircleChart.Changed();
        }

        public bool NavCanBackward()
        {
            return fCircleChart.NavCanBackward();
        }

        public bool NavCanForward()
        {
            return fCircleChart.NavCanForward();
        }

        public void NavNext()
        {
            fCircleChart.NavNext();
        }

        public void NavPrev()
        {
            fCircleChart.NavPrev();
        }

        public bool AllowQuickSearch()
        {
            return false;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            return new List<ISearchResult>();
        }

        public void QuickSearch()
        {
        }

        public void SelectByRec(GDMRecord record)
        {
        }

        public bool AllowFilter()
        {
            return false;
        }

        public void SetFilter()
        {
        }

        #endregion
    }
}
