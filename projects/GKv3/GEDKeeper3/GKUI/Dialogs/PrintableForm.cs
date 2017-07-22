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

//define DEBUG_PRINT

using Eto.Forms;
using GKCore.Interfaces;

namespace GKUI.Dialogs
{
    /// <summary>
    /// Form's class, common for the implementation of the print.
    /// </summary>
    public class PrintableForm : Form
    {
        private PrintDocument fPrintDoc;

        public PrintableForm()
        {
            InitPrintDoc();
        }

        protected virtual IPrintable GetPrintable()
        {
            return null; // dummy
        }

        #region Print support

        private void InitPrintDoc()
        {
            fPrintDoc = new PrintDocument();
            //fPrintDoc.QueryPageSettings += printDocument1_QueryPageSettings;
            //fPrintDoc.BeginPrint += printDocument1_BeginPrint;
            fPrintDoc.PrintPage += printDocument1_PrintPage;
        }

        private void InitCurDoc()
        {
            var printSettings = new PrintSettings();
            fPrintDoc.PrintSettings = printSettings;

            fPrintDoc.Name = Title;
            fPrintDoc.PrintSettings.Orientation = GetPrintable().IsLandscape() ? PageOrientation.Landscape : PageOrientation.Portrait;
            fPrintDoc.PrintSettings.MaximumPageRange = new Eto.Forms.Range<int>(1, 1);
            //fPrintDoc.DefaultPageSettings.Margins = new Printing.Margins(25, 25, 25, 25);
            fPrintDoc.PageCount = 1;
        }

        // FIXME: GKv3 DevRestriction
        /*private void printDocument1_BeginPrint(object sender, PrintEventArgs e)
        {
        }

        private void printDocument1_QueryPageSettings(object sender, QueryPageSettingsEventArgs e)
        {
            e.PageSettings.Landscape = GetPrintable().IsLandscape();
            e.PageSettings.Margins = new Printing.Margins(25, 25, 25, 25);
        }*/

        private void printDocument1_PrintPage(object sender, PrintPageEventArgs e)
        {
            /*Graphics gfx = e.Graphics;
            Rectangle marginBounds = e.MarginBounds;
            Rectangle pageBounds = e.PageBounds;

            #if DEBUG_PRINT
            gfx.DrawRectangle(Pens.Gray, marginBounds);
            #endif

            ImageHandler imgHandler = (ImageHandler)GetPrintable().GetPrintableImage();
            Image img = imgHandler.Handle;

            float imgW = img.Width;
            float imgH = img.Height;
            float factor = SysUtils.ZoomToFit(imgW, imgH, marginBounds.Width, marginBounds.Height);
            if (factor > 1.0f) factor = 1.0f;
            imgW = (imgW * factor);
            imgH = (imgH * factor);
            float x = (pageBounds.Width - imgW) / 2;
            float y = (pageBounds.Height - imgH) / 2;

            gfx.DrawImage(img, x, y, imgW, imgH);*/

            // Look at InitCurDoc()
            //e.HasMorePages = false;
        }

        public bool AllowPrint()
        {
            return true;
        }

        public void DoPrint()
        {
            InitCurDoc();

            using (PrintDialog printDlg = new PrintDialog()) {
                // Already includes all previous
                printDlg.ShowDialog(this, fPrintDoc);
            }
        }

        public void DoPrintPreview()
        {
            InitCurDoc();

            // It's not supported! :(
            /*using (PrintPreviewDialog previewDlg = new PrintPreviewDialog()) {
                previewDlg.WindowState = FormWindowState.Maximized;
                previewDlg.Document = fPrintDoc;
                previewDlg.ShowDialog();
            }*/
        }

        #endregion
    }
}
