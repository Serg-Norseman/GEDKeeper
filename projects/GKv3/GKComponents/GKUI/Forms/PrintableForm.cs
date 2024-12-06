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
