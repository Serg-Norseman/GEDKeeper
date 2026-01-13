/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Alex Zaytsev, Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
using System.IO.Compression;
using GDModel.Providers.GEDCOM;
using GKCore.Locales;
using GKCore.Utilities;

namespace GDModel.Providers.GEDZIP
{
    /// <summary>
    /// Processing the GEDZIP format is one part of the Genealogical Data Model (GDM).
    /// </summary>
    public class GEDZIPProvider : GEDCOMProvider
    {
        private const string GedcomEntry = "gedcom.ged";

        static GEDZIPProvider()
        {
            // Static initialization of the GEDCOMProvider is needed,
            // otherwise the standard tag identifiers are out of sync
            SysUtils.DoNotInline(GEDCOMFormats);
        }

        public GEDZIPProvider(GDMTree tree) : base(tree)
        {
        }

        public override string GetFilesFilter()
        {
            return LangMan.LS(LSID.GEDZIPFilter);
        }

        public override void LoadFromStream(Stream inputStream, bool charsetDetection = false)
        {
            using (var zip = new ZipArchive(inputStream, ZipArchiveMode.Read)) {
                var entry = GetArchiveEntry(zip, GedcomEntry);
                using (var entryStream = entry.Open()) {
                    // System.IO.Compression.DeflateStream -> CanSeek = false
                    // but ZipArchiveEntry has Length
                    InitProgress(entry.Length);
                    base.LoadFromStream(entryStream, charsetDetection);
                }
            }
        }

        public override void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
        {
            using (var zip = ZipFile.Open(fileName, ZipArchiveMode.Update)) {
                var entry = GetArchiveEntry(zip, GedcomEntry);
                using (var stream = entry.Open()) {
                    SaveToStreamExt(stream, charSet);
                    stream.Flush();
                }
            }
        }

        private static ZipArchiveEntry GetArchiveEntry(ZipArchive zip, string fileName)
        {
            return zip.GetEntry(fileName) ?? zip.CreateEntry(fileName);
        }
    }
}
