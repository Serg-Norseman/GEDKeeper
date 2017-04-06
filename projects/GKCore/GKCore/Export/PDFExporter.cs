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
using System.IO;
using System.Windows.Forms;

using GKCore.Interfaces;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class PDFExporter : Exporter
    {
        protected bool fAlbumPage;
        protected Padding fMargins;
        protected Document fDocument;
        protected PdfWriter fPdfWriter;

        protected PDFExporter(IBaseWindow baseWin) : base(baseWin)
        {
            fMargins = new Padding(20);
            fAlbumPage = true;
        }

        public override void Generate(bool show)
        {
            bool success = false;
            fPath = AppHub.StdDialogs.GetSaveFile("PDF files (*.pdf)|*.pdf");
            if (string.IsNullOrEmpty(fPath)) return;

            Rectangle pageSize = !fAlbumPage ? PageSize.A4 : PageSize.A4.Rotate();

            fDocument = new Document(pageSize, fMargins.Left, fMargins.Right, fMargins.Top, fMargins.Bottom);
            try
            {
                try
                {
                    fPdfWriter = PdfWriter.GetInstance(fDocument, new FileStream(fPath, FileMode.Create));
                    InternalGenerate();
                    success = true;
                }
                finally
                {
                    fDocument.Close();
                }
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("PDFExporter.Generate(): " + ex.Message);
                fBase.Host.LogWrite("PDFExporter.Generate(): " + ex.StackTrace);
            }

            #if !CI_MODE
            if (!success) {
                MessageBox.Show(LangMan.LS(LSID.LSID_GenerationFailed));
            } else {
                if (show) ShowResult();
            }
            #endif
        }

        protected abstract void InternalGenerate();

        //var line1 = new iTextSharp.text.pdf.draw.LineSeparator(0.0f, 100.0f, BaseColor.Black, Element.ALIGN_LEFT, 1);
        //pdfDoc.Add(New Chunk(line1));
    }
}
