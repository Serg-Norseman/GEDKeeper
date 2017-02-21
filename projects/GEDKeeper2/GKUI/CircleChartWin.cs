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
using System.Drawing;
using System.Drawing.Printing;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Charts;

namespace GKUI
{
    public partial class CircleChartWin : Form, IChartWindow
    {
        private readonly CircleChart fCircleChart;
        private readonly IBaseWindow fBaseWin;
        private readonly CircleChartType fType;

        private PrintDocument fPrintDoc;

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
            fType = type;

            if (type == CircleChartType.Ancestors) {
                fCircleChart = new AncestorsCircle(fBaseWin);
            } else {
                fCircleChart = new DescendantsCircle(fBaseWin);
            }

            fCircleChart.Name = "fCircleChart";
            fCircleChart.Dock = DockStyle.Fill;
            fCircleChart.NavRefresh += CircleChartWin_NavRefresh;
            fCircleChart.RootChanged += CircleChartWin_RootChanged;
            fCircleChart.RootPerson = startPerson;

            Controls.Add(fCircleChart);

            InitPrintDoc();

            SetLang();
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
            switch (e.KeyCode)
            {
                case Keys.Escape:
                    Close();
                    break;
            }
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fCircleChart.Select();
        }

        #region Print support

        private void InitPrintDoc()
        {
            fPrintDoc = new PrintDocument();
            fPrintDoc.QueryPageSettings += printDocument1_QueryPageSettings;
            fPrintDoc.BeginPrint += printDocument1_BeginPrint;
            fPrintDoc.PrintPage += printDocument1_PrintPage;
        }

        private void InitCurDoc()
        {
            fPrintDoc.DocumentName = Text;
            fPrintDoc.DefaultPageSettings.Landscape = fCircleChart.IsLandscape();
            fPrintDoc.DefaultPageSettings.Margins = new Margins(25, 25, 25, 25);
        }

        private void printDocument1_BeginPrint(object sender, PrintEventArgs e)
        {
        }

        private void printDocument1_QueryPageSettings(object sender, QueryPageSettingsEventArgs e)
        {
            e.PageSettings.Landscape = fCircleChart.IsLandscape();
            e.PageSettings.Margins = new Margins(25, 25, 25, 25);
        }

        private void printDocument1_PrintPage(object sender, PrintPageEventArgs e)
        {
            Graphics gfx = e.Graphics;
            Rectangle marginBounds = e.MarginBounds;
            Rectangle pageBounds = e.PageBounds;

            #if DEBUG_PRINT
            gfx.DrawRectangle(Pens.Gray, marginBounds);
            #endif

            Image img = fCircleChart.GetPrintableImage();

            int imgW = img.Width;
            int imgH = img.Height;
            float factor = SysUtils.ZoomToFit(imgW, imgH, marginBounds.Width, marginBounds.Height);
            if (factor > 1.0f) factor = 1.0f;
            imgW = (int)(imgW * factor);
            imgH = (int)(imgH * factor);
            int x = (pageBounds.Width - imgW) / 2;
            int y = (pageBounds.Height - imgH) / 2;

            gfx.DrawImage(img, x, y, imgW, imgH);

            e.HasMorePages = false;
        }

        #endregion

        #region ILocalization implementation
        
        public void SetLang()
        {
            if (fType == CircleChartType.Ancestors) {
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

        public bool AllowPrint()
        {
            return true;
        }

        public void DoPrint()
        {
            InitCurDoc();

            using (PrintDialog printDlg = new PrintDialog()) {
                printDlg.Document = fPrintDoc;

                if (printDlg.ShowDialog() == DialogResult.OK) {
                    fPrintDoc.PrinterSettings = printDlg.PrinterSettings;
                    fPrintDoc.Print();
                }
            }
        }

        public void DoPrintPreview()
        {
            InitCurDoc();

            using (PrintPreviewDialog previewDlg = new PrintPreviewDialog()) {
                previewDlg.WindowState = FormWindowState.Maximized;
                previewDlg.Document = fPrintDoc;
                previewDlg.ShowDialog();
            }
        }

        #endregion

        #region IWorkWindow implementation

        public string GetStatusString()
        {
            return string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fCircleChart.IndividualsCount.ToString());
        }

        public void UpdateView()
        {
            fCircleChart.Options.Assign(MainWin.Instance.Options.AncestorsCircleOptions);
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
