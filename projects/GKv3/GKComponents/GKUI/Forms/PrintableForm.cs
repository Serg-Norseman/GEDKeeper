/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

//define DEBUG_PRINT

using System;
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;

namespace GKUI.Forms
{
    /// <summary>
    /// Form's class, common for the implementation of the print.
    /// </summary>
    public class PrintableForm : StatusForm
    {
        private PrintDocument fPrintDoc;

        protected PrintableForm()
        {
            InitPrintDoc();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }

        #region Internal print support

        private void InitPrintDoc()
        {
            fPrintDoc = new PrintDocument();
            fPrintDoc.PrintPage += printDocument1_PrintPage;
        }

        private void InitCurDoc()
        {
            var printSettings = new PrintSettings();
            fPrintDoc.PrintSettings = printSettings;
            fPrintDoc.Name = Title;

            var printable = GetPrintable();
            if (printable != null) {
                fPrintDoc.PrintSettings.Orientation = printable.IsLandscape() ? PageOrientation.Landscape : PageOrientation.Portrait;
                fPrintDoc.PrintSettings.MaximumPageRange = new Eto.Forms.Range<int>(1, 1);
                fPrintDoc.PageCount = 1;
            }
        }

        private void printDocument1_PrintPage(object sender, PrintPageEventArgs e)
        {
            int pageMargin = 25;

            Graphics gfx = e.Graphics;
            SizeF pageSize = e.PageSize;

#if DEBUG_PRINT
            gfx.DrawRectangle(Pens.Gray, marginBounds);
#endif

            var printable = GetPrintable();
            if (printable != null) {
                Image img = ((ImageHandler)printable.GetPrintableImage()).Handle;

                float imgW = img.Width;
                float imgH = img.Height;
                float factor = GfxHelper.ZoomToFit(imgW, imgH, pageSize.Width - pageMargin * 2, pageSize.Height - pageMargin * 2);
                if (factor > 1.0f) factor = 1.0f;
                imgW = (imgW * factor);
                imgH = (imgH * factor);
                float x = (pageSize.Width - imgW) / 2;
                float y = (pageSize.Height - imgH) / 2;

                gfx.DrawImage(img, x, y, imgW, imgH);
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
                    printDlg.ShowDialog(this, fPrintDoc);
                }
            } catch (Exception ex) {
                Logger.WriteError("PrintableForm.DoPrint()", ex);
            }
        }

        public void DoPrintPreview()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.PrintPreview)) return;

            InitCurDoc();

            try {
                using (PrintPreviewDialog previewDlg = new PrintPreviewDialog(fPrintDoc)) {
                    previewDlg.ShowDialog(this);
                }
            } catch (Exception ex) {
                Logger.WriteError("PrintableForm.DoPrintPreview()", ex);
            }
        }

        #endregion
    }
}
