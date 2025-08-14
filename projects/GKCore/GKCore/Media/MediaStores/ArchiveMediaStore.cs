/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.IO;
using System.Text;
using BSLib;
using GKCore.Locales;

namespace GKCore.Media
{
    public sealed class ArchiveMediaStore : MediaStore
    {
        private readonly string fArchiveFileName;
        private readonly string fCompressedFileName;

        public override MediaStoreType StoreType { get { return MediaStoreType.mstArchive; } }


        public ArchiveMediaStore(BaseContext baseContext) : base(baseContext)
        {
        }

        public ArchiveMediaStore(BaseContext baseContext, string fileName) : base(baseContext, fileName)
        {
            fArchiveFileName = fBaseContext.GetArcFileName();
            fCompressedFileName = FileHelper.NormalizeFilename(this.FileName);
        }

        public override MediaStoreStatus VerifyMediaFile(out string displayFileName)
        {
            MediaStoreStatus result;

            try {
                displayFileName = this.FileName;

                if (!File.Exists(fArchiveFileName)) {
                    result = MediaStoreStatus.mssArcNotFound;
                } else {
                    bool fileExists;
                    using (ZipStorer zip = ZipStorer.Open(fArchiveFileName, FileAccess.Read, GetZipEncoding())) {
                        ZipStorer.ZipFileEntry entry = zip.FindFile(fCompressedFileName);
                        fileExists = (entry != null);
                    }
                    result = !fileExists ? MediaStoreStatus.mssFileNotFound : MediaStoreStatus.mssExists;
                }
            } catch (Exception ex) {
                Logger.WriteError("ArchiveMediaStore.VerifyMediaFile()", ex);
                displayFileName = this.FileName;
                result = MediaStoreStatus.mssBadData;
            }

            return result;
        }

        protected override Stream LoadMediaStream(bool throwException)
        {
            Stream resultStream = null;

            if (!File.Exists(fArchiveFileName)) {
                if (throwException) {
                    throw new MediaFileNotFoundException(fArchiveFileName);
                }
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
            } else {
                resultStream = new MemoryStream();
                ArcFileLoad(resultStream);
                resultStream.Seek(0, SeekOrigin.Begin);
            }

            return resultStream;
        }

        protected override string LoadMediaFile()
        {
            string resultFileName = GKUtils.GetTempDir() + Path.GetFileName(fCompressedFileName);

            if (!File.Exists(fArchiveFileName)) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
            } else {
                using (var fs = new FileStream(resultFileName, FileMode.Create, FileAccess.Write)) {
                    ArcFileLoad(fs);
                    fs.Flush();
                }
            }

            return resultFileName;
        }

        // TODO: Controlling the version of the GK GEDCOM file to determine the zip archive encoding!
        private static Encoding GetZipEncoding()
        {
            int treeVer = 0;
            return (treeVer == 0) ? Encoding.GetEncoding("CP866") : Encoding.UTF8;
        }

        private void ArcFileLoad(Stream toStream)
        {
            using (var zip = ZipStorer.Open(fArchiveFileName, FileAccess.Read, GetZipEncoding())) {
                var entry = zip.FindFile(fCompressedFileName);
                if (entry != null) {
                    zip.ExtractStream(entry, toStream);
                }
            }
        }

        protected override bool SaveCopy(string sourceFileName, string targetFileName)
        {
            bool result;
            try {
                ZipStorer zip = null;
                try {
                    string arcFn = fBaseContext.GetArcFileName();
                    if (File.Exists(arcFn)) {
                        zip = ZipStorer.Open(arcFn, FileAccess.ReadWrite, GetZipEncoding());
                    } else {
                        zip = ZipStorer.Create(arcFn, "");
                    }
                    zip.AddFile(ZipStorer.Compression.Deflate, sourceFileName, targetFileName, null);
                } finally {
                    if (zip != null) zip.Dispose();
                }
                result = true;
            } catch (IOException ex) {
                Logger.WriteError(string.Format("ArchiveMediaStore.SaveCopy({0}, {1})", sourceFileName, targetFileName), ex);
                result = false;
            }
            return result;
        }

        protected override void DeleteFile()
        {
            using (var zip = ZipStorer.Open(fArchiveFileName, FileAccess.Read, GetZipEncoding())) {
                var entry = zip.FindFile(fCompressedFileName);
                if (entry != null) {
                    var zfes = new List<ZipStorer.ZipFileEntry>() { entry };
                    ZipStorer.RemoveEntries(zip, zfes);
                }
            }
        }
    }
}
