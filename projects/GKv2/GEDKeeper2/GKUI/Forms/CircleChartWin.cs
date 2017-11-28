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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Charts;
using GKUI.Forms;

namespace GKUI.Forms
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

            tbImageSave.Image = GKResources.iSaveImage;
            tbDocPreview.Image = GKResources.iPreview;
            tbDocPrint.Image = GKResources.iPrint;
            tbPrev.Image = GKResources.iLeft1;
            tbNext.Image = GKResources.iRight1;

            ToolBar1.Visible = true;

            fBaseWin = baseWin;

            fCircleChart = new CircleChart();
            fCircleChart.Base = fBaseWin;
            fCircleChart.ChartType = type;
            fCircleChart.Name = "fCircleChart";
            fCircleChart.Dock = DockStyle.Fill;
            fCircleChart.NavRefresh += CircleChartWin_NavRefresh;
            fCircleChart.RootChanged += CircleChartWin_RootChanged;
            fCircleChart.RootPerson = startPerson;
            Controls.Add(fCircleChart);
            Controls.SetChildIndex(fCircleChart, 0);

            SetLang();
        }

        protected override IPrintable GetPrintable()
        {
            return fCircleChart;
        }

        private void UpdateNavControls()
        {
            try
            {
                tbPrev.Enabled = NavCanBackward();
                tbNext.Enabled = NavCanForward();
            } catch (Exception ex) {
                Logger.LogWrite("CircleChartWin.UpdateNavControls(): " + ex.Message);
            }
        }

        private void CircleChartWin_NavRefresh(object sender, EventArgs e)
        {
            AppHost.Instance.UpdateControls(false);
            UpdateNavControls();
        }

        private void CircleChartWin_RootChanged(object sender, GEDCOMIndividualRecord person)
        {
            AppHost.Instance.UpdateControls(false);
            UpdateNavControls();
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

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fCircleChart.Select();
            AppHost.Instance.UpdateControls(false);
        }

        // TODO: update localization and GKv3
        private void tbImageSave_Click(object sender, EventArgs e)
        {
            string filters = LangMan.LS(LSID.LSID_TreeImagesFilter) + "|SVG files (*.svg)|*.svg";
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", filters, 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName)) {
                fCircleChart.SaveSnapshot(fileName);
            }
        }

        private void tbDocPreview_Click(object sender, EventArgs e)
        {
            DoPrintPreview();
        }

        private void tbDocPrint_Click(object sender, EventArgs e)
        {
            DoPrint();
        }

        #region ILocalization implementation

        public void SetLang()
        {
            if (fCircleChart.ChartType == CircleChartType.Ancestors) {
                Text = LangMan.LS(LSID.LSID_AncestorsCircle);
            } else {
                Text = LangMan.LS(LSID.LSID_DescendantsCircle);
            }

            tbImageSave.ToolTipText = LangMan.LS(LSID.LSID_ImageSaveTip);
            tbDocPrint.ToolTipText = LangMan.LS(LSID.LSID_DocPrint);
            tbDocPreview.ToolTipText = LangMan.LS(LSID.LSID_DocPreview);
        }

        #endregion

        #region IChartWindow implementation

        public void GenChart()
        {
            AppHost.Instance.UpdateControls(false);
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
            return new List<ISearchResult>();
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
