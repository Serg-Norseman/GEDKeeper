/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
            get { return this.fBaseWin; }
        }

        public CircleChartWin(IBaseWindow baseWin, GEDCOMIndividualRecord startPerson, CircleChartType type)
        {
            this.InitializeComponent();
            this.MdiParent = MainWin.Instance;
            this.ShowInTaskbar = true;

            this.fBaseWin = baseWin;
            this.fType = type;

            if (type == CircleChartType.Ancestors) {
                this.fCircleChart = new AncestorsCircle(this.fBaseWin);
            } else {
                this.fCircleChart = new DescendantsCircle(this.fBaseWin);
            }

            this.fCircleChart.Name = "fCircleChart";
            this.fCircleChart.Dock = DockStyle.Fill;
            this.fCircleChart.NavRefresh += CircleChartWin_NavRefresh;
            this.fCircleChart.RootChanged += CircleChartWin_RootChanged;
            this.fCircleChart.RootPerson = startPerson;

            this.Controls.Add(this.fCircleChart);

            this.InitPrintDoc();

            this.SetLang();
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
                    base.Close();
                    break;
            }
        }

        #region Print support

        private void InitPrintDoc()
        {
            this.fPrintDoc = new PrintDocument();
            this.fPrintDoc.QueryPageSettings += printDocument1_QueryPageSettings;
            this.fPrintDoc.BeginPrint += printDocument1_BeginPrint;
            this.fPrintDoc.PrintPage += printDocument1_PrintPage;
        }

        private void InitCurDoc()
        {
            this.fPrintDoc.DocumentName = this.Text;
            this.fPrintDoc.DefaultPageSettings.Landscape = fCircleChart.IsLandscape();
            this.fPrintDoc.DefaultPageSettings.Margins = new Margins(25, 25, 25, 25);
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
            if (this.fType == CircleChartType.Ancestors) {
                this.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
            } else {
                this.Text = LangMan.LS(LSID.LSID_DescendantsCircle);
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
            this.InitCurDoc();

            using (PrintDialog printDlg = new PrintDialog()) {
                printDlg.Document = this.fPrintDoc;

                if (printDlg.ShowDialog() == DialogResult.OK) {
                    this.fPrintDoc.PrinterSettings = printDlg.PrinterSettings;
                    this.fPrintDoc.Print();
                }
            }
        }

        public void DoPrintPreview()
        {
            this.InitCurDoc();

            using (PrintPreviewDialog previewDlg = new PrintPreviewDialog()) {
                previewDlg.WindowState = FormWindowState.Maximized;
                previewDlg.Document = this.fPrintDoc;
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
            this.fCircleChart.Options.Assign(MainWin.Instance.Options.AncestorsCircleOptions);
            this.fCircleChart.Changed();
        }

        public bool NavCanBackward()
        {
            return this.fCircleChart.NavCanBackward();
        }

        public bool NavCanForward()
        {
            return this.fCircleChart.NavCanForward();
        }

        public void NavNext()
        {
            this.fCircleChart.NavNext();
        }

        public void NavPrev()
        {
            this.fCircleChart.NavPrev();
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
