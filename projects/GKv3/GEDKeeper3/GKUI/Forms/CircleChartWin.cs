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
using Eto.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class CircleChartWin : PrintableForm, IChartWindow, ICircleChartWin
    {
        private readonly CircleChartWinController fController;

        private readonly IBaseWindow fBaseWin;
        private readonly CircleChart fCircleChart;

        public IBaseWindow Base
        {
            get { return fBaseWin; }
        }

        public CircleChartWin(IBaseWindow baseWin, GEDCOMIndividualRecord startPerson, CircleChartType type)
        {
            InitializeComponent();

            fBaseWin = baseWin;

            fCircleChart = new CircleChart();
            fCircleChart.Base = fBaseWin;
            fCircleChart.ChartType = type;
            fCircleChart.NavRefresh += CircleChartWin_NavRefresh;
            fCircleChart.ZoomChanged += CircleChartWin_NavRefresh;
            fCircleChart.RootChanged += CircleChartWin_RootChanged;
            fCircleChart.RootPerson = startPerson;
            fCircleChart.Options.Assign(GlobalOptions.Instance.AncestorsCircleOptions);
            Content = fCircleChart;

            SetLang();

            fController = new CircleChartWinController(this);
            fController.Init(fBaseWin);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fCircleChart.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fCircleChart.Focus();
            UpdateControls();
        }

        protected override IPrintable GetPrintable()
        {
            return fCircleChart;
        }

        private void CircleChartWin_NavRefresh(object sender, EventArgs e)
        {
            GenChart();
        }

        private void CircleChartWin_RootChanged(object sender, GEDCOMIndividualRecord person)
        {
            GenChart();
        }

        private void CircleChartWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
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

        private void tbOptions_Click(object sender, EventArgs e)
        {
            using (OptionsDlg dlgOptions = new OptionsDlg(AppHost.Instance))
            {
                dlgOptions.SetPage(OptionsPage.opAncestorsCircle);
                if (dlgOptions.ShowModal() == DialogResult.Ok) {
                    AppHost.Instance.ApplyOptions();
                }
            }
        }

        #region ILocalization implementation

        public void SetLang()
        {
            if (fCircleChart.ChartType == CircleChartType.Ancestors) {
                Title = LangMan.LS(LSID.LSID_AncestorsCircle);
            } else {
                Title = LangMan.LS(LSID.LSID_DescendantsCircle);
            }

            tbImageSave.ToolTip = LangMan.LS(LSID.LSID_ImageSaveTip);
            tbDocPrint.ToolTip = LangMan.LS(LSID.LSID_DocPrint);
            tbDocPreview.ToolTip = LangMan.LS(LSID.LSID_DocPreview);
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
                StatusLines[0] = string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fCircleChart.Model.IndividualsCount);
                var imageSize = fCircleChart.GetImageSize();
                StatusLines[1] = string.Format(LangMan.LS(LSID.LSID_ImageSize), imageSize.Width, imageSize.Height);

                tbPrev.Enabled = NavCanBackward();
                tbNext.Enabled = NavCanForward();

                AppHost.Instance.UpdateControls(false, true);
            } catch (Exception ex) {
                Logger.LogWrite("CircleChartWin.UpdateControls(): " + ex.Message);
            }
        }

        public void UpdateSettings()
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
