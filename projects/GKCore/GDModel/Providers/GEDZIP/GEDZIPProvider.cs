/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Alex Zaytsev, Sergey V. Zhdanovskih.
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

using System.IO;
using System.IO.Compression;
using GDModel.Providers.GEDCOM;
using GKCore;

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
            return "GEDZIP files (*.gdz,*.zip)|*.gdz,*.zip";
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
