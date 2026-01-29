/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using BSLib;
using GKCore.Design;
using GKCore.Export.Formats;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ReportExporter : Exporter
    {
        private bool fAlbumPage;
        protected int fDefImageHeight;
        protected int fDefImageWidth;
        protected bool fPDFOnly;
        protected string fTitle;


        protected ReportExporter(IBaseWindow baseWin, bool albumPage)
            : base(baseWin)
        {
            fAlbumPage = albumPage;

            fDefImageHeight = 320;
            fDefImageWidth = 200;

            fPDFOnly = false;
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
            string availableFormats = "";
            if (!fPDFOnly) {
                availableFormats = LangMan.LS(LSID.HTMLFilter) + "|" + LangMan.LS(LSID.RTFFilter);
                availableFormats += "|";
            }
            availableFormats += LangMan.LS(LSID.PDFFilter);

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
#if !TERM
                writer = new PDFWriter();
#else
                return;
#endif
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
