/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ReportExporter : Exporter
    {
        protected string fTitle;
        private bool fAlbumPage;

        protected int fDefImageHeight;
        protected int fDefImageWidth;

        protected ReportExporter(IBaseWindow baseWin, bool albumPage)
            : base(baseWin)
        {
            fAlbumPage = albumPage;

            fDefImageHeight = 320;
            fDefImageWidth = 200;
        }

        protected abstract void InternalGenerate();

        public bool Generate(CustomWriter writer)
        {
            bool result = false;

            try {
                fWriter = writer;
                fWriter.SetAlbumPage(fAlbumPage);
                fWriter.SetDocumentTitle(fTitle);
                fWriter.SetFileName(fPath);

                fWriter.BeginWrite();
                try {
                    InternalGenerate();
                    result = true;
                } finally {
                    fWriter.EndWrite();
                }
            } catch (Exception ex) {
                Logger.WriteError("ReportExporter.Generate()", ex);
            }

            return result;
        }

        public async override void Generate(bool show)
        {
            string availableFormats = LangMan.LS(LSID.HTMLFilter) + "|" + LangMan.LS(LSID.RTFFilter);
            availableFormats += "|" + LangMan.LS(LSID.PDFFilter);

            fPath = await AppHost.StdDialogs.GetSaveFile(GlobalOptions.Instance.ReportExportLastDir, availableFormats);
            if (string.IsNullOrEmpty(fPath)) return;

            GlobalOptions.Instance.ReportExportLastDir = Path.GetDirectoryName(fPath);

            string ext = FileHelper.GetFileExtension(fPath);

            CustomWriter writer;
            if (string.Equals(ext, ".html")) {
                writer = new HTMLWriter();
            } else if (string.Equals(ext, ".rtf")) {
                writer = new RTFWriter();
            } else {
                writer = new PDFWriter();
            }

            bool success = Generate(writer);

            #if !CI_MODE
            if (!success) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.GenerationFailed));
            } else {
                if (show) ShowResult();
            }
            #endif
        }
    }
}
