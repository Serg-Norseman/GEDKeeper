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

//define DEBUG_PRINT

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Printing;
using System.Windows.Forms;
using BSLib;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Options;
using GKUI.Components;
using GKUI.Platform.Handlers;

namespace GKUI.Forms
{
    /// <summary>
    /// Form's class, common for the implementation of the print.
    /// </summary>
    public class PrintableForm : StatusForm
    {
        private const int PageMargin = 20;

        private bool fMultipage;
        private int fPageNumber;
        private Rectangle fPageBounds;
        private List<ExtRect> fPages;
        private Image fPrintableImage;
        private Rectangle fPrintBounds;
        private PrintDocument fPrintDoc;
        private float fScaleFactor;

        protected PrintableForm()
        {
            fPrintDoc = new PrintDocument();
            fPrintDoc.PrintPage += printDocument1_PrintPage;

            fPages = new List<ExtRect>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fPrintDoc.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Internal print support

        private void InitCurDoc()
        {
            bool multipage = GlobalOptions.Instance.MultipagePrint;

            fPrintableImage = null;
            fPages.Clear();
            fPageNumber = 0;

            fPrintDoc.DocumentName = Text;
            var printable = GetPrintable();
            if (printable != null) {
                var pageSettings = fPrintDoc.DefaultPageSettings;
                pageSettings.Landscape = printable.IsLandscape();
                pageSettings.Margins = new Margins(PageMargin, PageMargin, PageMargin, PageMargin);

                fPrintableImage = ((ImageHandler)printable.GetPrintableImage()).Handle;

                fPageBounds = pageSettings.Bounds;
                fPrintBounds = fPageBounds;
                fPrintBounds.Inflate(-PageMargin, -PageMargin);

                fScaleFactor = GfxHelper.ZoomToFit(fPrintableImage.Width, fPrintableImage.Height, fPrintBounds.Width, fPrintBounds.Height);
                if (fScaleFactor > 1.0f) fScaleFactor = 1.0f;

                fMultipage = multipage && (fScaleFactor < 1.0f);

                if (fMultipage) {
                    GKUtils.SplitImage(fPages, fPrintableImage.Width, fPrintableImage.Height, fPrintBounds.Width, fPrintBounds.Height);
                }
            }
        }

        private void printDocument1_PrintPage(object sender, PrintPageEventArgs e)
        {
            Graphics gfx = e.Graphics;
            Rectangle marginBounds = e.MarginBounds;
            Rectangle pageBounds = e.PageBounds;

            //int hardMarginX = (int)(e.PageSettings.HardMarginX * 100 / gfx.DpiX);
            //int hardMarginY = (int)(e.PageSettings.HardMarginY * 100 / gfx.DpiY);
            //var printBounds = e.MarginBounds; //new Rectangle(hardMarginX, hardMarginY, pageBounds.Width - (hardMarginX * 2), pageBounds.Height - (hardMarginY * 2));

#if DEBUG_PRINT
            gfx.DrawRectangle(Pens.Red, marginBounds);
#endif

            if (fPrintableImage != null) {
                if (fMultipage) {
                    var partBounds = UIHelper.Rt2Rt(fPages[fPageNumber]);
                    gfx.DrawImage(fPrintableImage, fPrintBounds.X, fPrintBounds.Y, partBounds, GraphicsUnit.Pixel);

                    // clipping border (may be option?)
                    var clipBounds = new Rectangle(fPrintBounds.X, fPrintBounds.Y, partBounds.Width, partBounds.Height);
                    gfx.DrawRectangle(Pens.LightGray, clipBounds);

                    e.HasMorePages = (++fPageNumber < fPages.Count);
                } else {
                    float imgW = (fPrintableImage.Width * fScaleFactor);
                    float imgH = (fPrintableImage.Height * fScaleFactor);
                    float x = (fPageBounds.Width - imgW) / 2;
                    float y = (fPageBounds.Height - imgH) / 2;
                    gfx.DrawImage(fPrintableImage, x, y, imgW, imgH);
                    e.HasMorePages = false;
                }
            }
        }

        #endregion

        #region Public interface

        protected virtual IPrintable GetPrintable()
        {
            return null;
        }

        public bool AllowPrint()
        {
            return true;
        }

        public void DoPrint()
        {
            InitCurDoc();

            try {
                using (PrintDialog printDlg = new PrintDialog()) {
                    printDlg.Document = fPrintDoc;

                    if (printDlg.ShowDialog() == DialogResult.OK) {
                        fPrintDoc.PrinterSettings = printDlg.PrinterSettings;
                        fPrintDoc.Print();
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("PrintableForm.DoPrint()", ex);
            }
        }

        public void DoPrintPreview()
        {
            InitCurDoc();

            try {
                using (PrintPreviewDialog previewDlg = new PrintPreviewDialog()) {
                    previewDlg.WindowState = FormWindowState.Maximized;
                    previewDlg.Document = fPrintDoc;
                    previewDlg.ShowDialog();
                }
            } catch (Exception ex) {
                Logger.WriteError("PrintableForm.DoPrintPreview()", ex);
            }
        }

        #endregion
    }
}
