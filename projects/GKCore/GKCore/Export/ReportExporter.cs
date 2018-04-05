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
using BSLib;
using GKCore.Interfaces;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ReportExporter : Exporter
    {
        protected string fTitle;

        protected ReportExporter(IBaseWindow baseWin)
            : base(baseWin)
        {
        }

        protected abstract void InternalGenerate();

        public bool Generate(CustomWriter writer)
        {
            bool result = false;

            try {
                fWriter = writer;
                fWriter.SetAlbumPage(false);
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
                Logger.LogWrite("ReportExporter.Generate(): " + ex.Message);
                Logger.LogWrite("ReportExporter.Generate(): " + ex.StackTrace);
            }

            return result;
        }

        public override void Generate(bool show)
        {
            string availableFormats = LangMan.LS(LSID.LSID_HTMLFilter) + "|" + LangMan.LS(LSID.LSID_RTFFilter);
            availableFormats += "|" + LangMan.LS(LSID.LSID_PDFFilter);

            fPath = AppHost.StdDialogs.GetSaveFile(availableFormats);
            if (string.IsNullOrEmpty(fPath)) return;

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
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_GenerationFailed));
            } else {
                if (show) ShowResult();
            }
            #endif
        }
    }
}
