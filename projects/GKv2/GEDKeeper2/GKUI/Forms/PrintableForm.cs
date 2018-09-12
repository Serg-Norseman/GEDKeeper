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

//define DEBUG_PRINT

using System.Drawing;
using System.Drawing.Printing;
using System.Windows.Forms;

using BSLib;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// Form's class, common for the implementation of the print.
    /// </summary>
    public class PrintableForm : StatusForm
    {
        private PrintDocument fPrintDoc;

        public PrintableForm() : base()
        {
            InitPrintDoc();
        }

        #region Internal print support

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
            var printable = GetPrintable();
            if (printable != null) {
                fPrintDoc.DefaultPageSettings.Landscape = printable.IsLandscape();
                fPrintDoc.DefaultPageSettings.Margins = new System.Drawing.Printing.Margins(25, 25, 25, 25);
            }
        }

        private void printDocument1_BeginPrint(object sender, PrintEventArgs e)
        {
        }

        private void printDocument1_QueryPageSettings(object sender, QueryPageSettingsEventArgs e)
        {
            var printable = GetPrintable();
            if (printable != null) {
                e.PageSettings.Landscape = printable.IsLandscape();
                e.PageSettings.Margins = new System.Drawing.Printing.Margins(25, 25, 25, 25);
            }
        }

        private void printDocument1_PrintPage(object sender, PrintPageEventArgs e)
        {
            Graphics gfx = e.Graphics;
            Rectangle marginBounds = e.MarginBounds;
            Rectangle pageBounds = e.PageBounds;

            #if DEBUG_PRINT
            gfx.DrawRectangle(Pens.Gray, marginBounds);
            #endif

            var printable = GetPrintable();
            if (printable != null) {
                ImageHandler imgHandler = (ImageHandler)printable.GetPrintableImage();
                Image img = imgHandler.Handle;

                float imgW = img.Width;
                float imgH = img.Height;
                float factor = GfxHelper.ZoomToFit(imgW, imgH, marginBounds.Width, marginBounds.Height);
                if (factor > 1.0f) factor = 1.0f;
                imgW = (imgW * factor);
                imgH = (imgH * factor);
                float x = (pageBounds.Width - imgW) / 2;
                float y = (pageBounds.Height - imgH) / 2;

                gfx.DrawImage(img, x, y, imgW, imgH);
            }

            e.HasMorePages = false;
        }

        #endregion

        #region Public interface

        protected virtual IPrintable GetPrintable()
        {
            return null; // dummy
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
    }
}
