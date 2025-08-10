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
        public ArchiveMediaStore(IBaseContext baseContext, MediaStoreType storeType, string fileName) : base(baseContext, storeType, fileName)
        {
        }


        public override MediaStoreStatus VerifyMediaFile(out string fileName)
        {
            MediaStoreStatus result = MediaStoreStatus.mssBadData;

            try {
                fileName = this.FileName;

                if (!File.Exists(fBaseContext.GetArcFileName())) {
                    result = MediaStoreStatus.mssArcNotFound;
                } else {
                    if (!ArcFileExists(fBaseContext, fileName)) {
                        result = MediaStoreStatus.mssFileNotFound;
                    } else {
                        result = MediaStoreStatus.mssExists;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("ArchiveMediaStore.VerifyMediaFile()", ex);
                fileName = string.Empty;
            }

            return result;
        }

        protected override Stream LoadMediaStream(bool throwException)
        {
            string compressedFileName = this.FileName;

            var resultStream = new MemoryStream();

            string arcFile = fBaseContext.GetArcFileName();
            if (!File.Exists(arcFile)) {
                if (throwException) {
                    throw new MediaFileNotFoundException(arcFile);
                }
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
            } else {
                ArcFileLoad(fBaseContext, compressedFileName, resultStream);
                resultStream.Seek(0, SeekOrigin.Begin);
            }

            return resultStream;
        }

        protected override string LoadMediaFile()
        {
            string compressedFileName = this.FileName;

            string resultFileName = GKUtils.GetTempDir() + Path.GetFileName(compressedFileName);

            using (var fs = new FileStream(resultFileName, FileMode.Create, FileAccess.Write)) {
                if (!File.Exists(fBaseContext.GetArcFileName())) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
                } else {
                    ArcFileLoad(fBaseContext, compressedFileName, fs);
                }
                fs.Flush();
            }

            return resultFileName;
        }

        // TODO: Controlling the version of the GK GEDCOM file to determine the zip archive encoding!
        private static Encoding GetZipEncoding()
        {
            int treeVer = 0;
            return (treeVer == 0) ? Encoding.GetEncoding("CP866") : Encoding.UTF8;
        }

        private static void ArcFileLoad(IBaseContext baseContext, string targetFn, Stream toStream)
        {
            targetFn = FileHelper.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(baseContext.GetArcFileName(), FileAccess.Read, GetZipEncoding())) {
                ZipStorer.ZipFileEntry entry = zip.FindFile(targetFn);
                if (entry != null) {
                    zip.ExtractStream(entry, toStream);
                }
            }
        }

        public static void ArcFileSave(IBaseContext baseContext, string fileName, string sfn)
        {
            string arcFn = baseContext.GetArcFileName();
            ZipStorer zip = null;

            try {
                if (File.Exists(arcFn)) {
                    zip = ZipStorer.Open(arcFn, FileAccess.ReadWrite, GetZipEncoding());
                } else {
                    zip = ZipStorer.Create(arcFn, "");
                }
                zip.AddFile(ZipStorer.Compression.Deflate, fileName, sfn, null);
            } finally {
                if (zip != null) zip.Dispose();
            }
        }

        private static bool ArcFileExists(IBaseContext baseContext, string targetFn)
        {
            targetFn = FileHelper.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(baseContext.GetArcFileName(), FileAccess.Read, GetZipEncoding())) {
                ZipStorer.ZipFileEntry entry = zip.FindFile(targetFn);
                return (entry != null);
            }
        }

        protected override void DeleteFile(string fileName)
        {
            fileName = FileHelper.NormalizeFilename(fileName);

            using (ZipStorer zip = ZipStorer.Open(fBaseContext.GetArcFileName(), FileAccess.Read, GetZipEncoding())) {
                ZipStorer.ZipFileEntry entry = zip.FindFile(fileName);
                if (entry != null) {
                    var zfes = new List<ZipStorer.ZipFileEntry>();
                    zfes.Add(entry);
                    // TODO: optimize this method!
                    ZipStorer.RemoveEntries(zip, zfes);
                }
            }
        }
    }
}
