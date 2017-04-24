/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Charts;
using GKUI.Components;

namespace GKUI
{
    public partial class CircleChartWin : PrintableForm, IChartWindow
    {
        private readonly CircleChart fCircleChart;
        private readonly IBaseWindow fBaseWin;

        public IBaseWindow Base
        {
            get { return fBaseWin; }
        }

        public CircleChartWin(IBaseWindow baseWin, GEDCOMIndividualRecord startPerson, CircleChartType type)
        {
            InitializeComponent();
            MdiParent = MainWin.Instance;
            ShowInTaskbar = true;

            fBaseWin = baseWin;

            fCircleChart = new CircleChart(fBaseWin);
            fCircleChart.ChartType = type;
            fCircleChart.Name = "fCircleChart";
            fCircleChart.Dock = DockStyle.Fill;
            fCircleChart.NavRefresh += CircleChartWin_NavRefresh;
            fCircleChart.RootChanged += CircleChartWin_RootChanged;
            fCircleChart.RootPerson = startPerson;
            Controls.Add(fCircleChart);

            SetLang();
        }

        protected override IPrintable GetPrintable()
        {
            return fCircleChart;
        }

        private void CircleChartWin_NavRefresh(object sender, EventArgs e)
        {
            MainWin.Instance.UpdateControls(false);
        }

        private void CircleChartWin_RootChanged(object sender, GEDCOMIndividualRecord person)
        {
            MainWin.Instance.UpdateControls(false);
        }

        private void CircleChartWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fCircleChart.Select();
        }

        #region ILocalization implementation

        public void SetLang()
        {
            if (fCircleChart.ChartType == CircleChartType.Ancestors) {
                Text = LangMan.LS(LSID.LSID_AncestorsCircle);
            } else {
                Text = LangMan.LS(LSID.LSID_DescendantsCircle);
            }
        }

        #endregion

        #region IChartWindow implementation

        public void GenChart(bool show)
        {
            if (show) base.Show();

            MainWin.Instance.UpdateControls(false);
        }

        #endregion

        #region IWorkWindow implementation

        public string GetStatusString()
        {
            return string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fCircleChart.Model.IndividualsCount.ToString());
        }

        public void UpdateView()
        {
            fCircleChart.Options.Assign(GlobalOptions.Instance.AncestorsCircleOptions);
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
            return null;
        }

        public void QuickSearch()
        {
        }

        public void SelectByRec(GEDCOMIndividualRecord iRec)
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
