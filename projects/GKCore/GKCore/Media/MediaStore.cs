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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Media
{
    /// <summary>
    /// 
    /// </summary>
    public class MediaStore
    {
        private readonly IBaseContext fBaseContext;

        public MediaStoreType StoreType { get; private set; }
        public string FileName { get; private set; }


        public MediaStore(IBaseContext baseContext, MediaStoreType storeType, string fileName)
        {
            fBaseContext = baseContext;
            StoreType = storeType;
            FileName = fileName;
        }


        public static MediaStoreType GetStoreTypeEx(string fileReference)
        {
            if (fileReference == null)
                throw new ArgumentNullException(nameof(fileReference));

            MediaStoreType result = MediaStoreType.mstReference;
            for (int i = 1; i <= 4; i++) {
                if (fileReference.StartsWith(GKData.GKStoreTypes[i].Sign, StringComparison.Ordinal)) {
                    result = (MediaStoreType)i;
                    break;
                }
            }
            return result;
        }

        public static MediaStore GetStoreType(IBaseContext baseContext, string fileReference)
        {
            if (string.IsNullOrEmpty(fileReference))
                throw new ArgumentNullException(nameof(fileReference));

            string fileName = fileReference;
            MediaStoreType storeType = GetStoreTypeEx(fileName);

            if (storeType != MediaStoreType.mstReference && storeType != MediaStoreType.mstURL) {
                fileName = fileName.Remove(0, 4);
            }

            return new MediaStore(baseContext, storeType, fileName);
        }

        // TODO: Controlling the version of the GK GEDCOM file to determine the zip archive encoding!
        public static Encoding GetZipEncoding()
        {
            int treeVer = 0;
            return (treeVer == 0) ? Encoding.GetEncoding("CP866") : Encoding.UTF8;
        }


        public static void ArcFileLoad(IBaseContext baseContext, string targetFn, Stream toStream)
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

        public static void ArcFileDelete(IBaseContext baseContext, string targetFn)
        {
            targetFn = FileHelper.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(baseContext.GetArcFileName(), FileAccess.Read, GetZipEncoding())) {
                ZipStorer.ZipFileEntry entry = zip.FindFile(targetFn);
                if (entry != null) {
                    var zfes = new List<ZipStorer.ZipFileEntry>();
                    zfes.Add(entry);
                    // TODO: optimize this method!
                    ZipStorer.RemoveEntries(zip, zfes);
                }
            }
        }

        public static bool ArcFileExists(IBaseContext baseContext, string targetFn)
        {
            targetFn = FileHelper.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(baseContext.GetArcFileName(), FileAccess.Read, GetZipEncoding())) {
                ZipStorer.ZipFileEntry entry = zip.FindFile(targetFn);
                return (entry != null);
            }
        }


        public MediaStoreStatus VerifyMediaFile(out string fileName)
        {
            MediaStoreStatus result = MediaStoreStatus.mssBadData;

            try {
                fileName = this.FileName;

                switch (this.StoreType) {
                    case MediaStoreType.mstStorage:
                        string stgPath = fBaseContext.GetStgFolder(false);
                        if (!Directory.Exists(stgPath)) {
                            result = MediaStoreStatus.mssStgNotFound;
                        } else {
                            fileName = stgPath + fileName;
                            if (!File.Exists(fileName)) {
                                result = MediaStoreStatus.mssFileNotFound;
                            } else {
                                result = MediaStoreStatus.mssExists;
                            }
                        }
                        break;

                    case MediaStoreType.mstArchive:
                        if (!File.Exists(fBaseContext.GetArcFileName())) {
                            result = MediaStoreStatus.mssArcNotFound;
                        } else {
                            if (!ArcFileExists(fBaseContext, fileName)) {
                                result = MediaStoreStatus.mssFileNotFound;
                            } else {
                                result = MediaStoreStatus.mssExists;
                            }
                        }
                        break;

                    case MediaStoreType.mstReference:
                    case MediaStoreType.mstRelativeReference:
                        if (StoreType == MediaStoreType.mstRelativeReference) {
                            fileName = fBaseContext.GetTreePath(fBaseContext.FileName) + fileName;
                        }
                        if (!File.Exists(fileName)) {
                            string xFileName = FileHelper.NormalizeFilename(fileName);
                            if (!File.Exists(xFileName)) {
                                result = MediaStoreStatus.mssFileNotFound;
                            } else {
                                result = MediaStoreStatus.mssExists;
                                fileName = xFileName;
                            }
                        } else {
                            result = MediaStoreStatus.mssExists;
                        }
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.VerifyMediaFile()", ex);
                fileName = string.Empty;
            }

            return result;
        }

        public Stream MediaLoad(bool throwException)
        {
            Stream stream = null;

            string targetFn = this.FileName;
            MediaStoreType mst = this.StoreType;

            if (fBaseContext.IsGEDZIP() && this.StoreType == MediaStoreType.mstReference) {
                mst = MediaStoreType.mstArchive;
            }

            switch (mst) {
                case MediaStoreType.mstStorage:
                    targetFn = fBaseContext.GetStgFolder(false) + targetFn;
                    if (!File.Exists(targetFn)) {
                        if (throwException) {
                            throw new MediaFileNotFoundException(targetFn);
                        }

                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
                    } else {
                        stream = new FileStream(targetFn, FileMode.Open, FileAccess.Read);
                    }
                    break;

                case MediaStoreType.mstArchive:
                    stream = new MemoryStream();
                    string arcFile = fBaseContext.GetArcFileName();
                    if (!File.Exists(arcFile)) {
                        if (throwException) {
                            throw new MediaFileNotFoundException(arcFile);
                        }

                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
                    } else {
                        ArcFileLoad(fBaseContext, targetFn, stream);
                        stream.Seek(0, SeekOrigin.Begin);
                    }
                    break;

                case MediaStoreType.mstRelativeReference:
                case MediaStoreType.mstReference:
                    if (mst == MediaStoreType.mstRelativeReference) {
                        string treeName = fBaseContext.FileName;
                        targetFn = fBaseContext.GetTreePath(treeName) + targetFn;
                    }
                    if (!File.Exists(targetFn)) {
                        if (throwException) {
                            throw new MediaFileNotFoundException(targetFn);
                        }
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileNotFound, targetFn));
                    } else {
                        stream = new FileStream(targetFn, FileMode.Open, FileAccess.Read);
                    }
                    break;

                case MediaStoreType.mstURL:
                    stream = GKUtils.GetWebStream(targetFn);
                    break;
            }

            return stream;
        }

        public string MediaLoad()
        {
            string fileName = string.Empty;

            try {
                var mst = this.StoreType;
                if (mst != MediaStoreType.mstURL && !VerifyMediaFileWM()) {
                    return string.Empty;
                }

                if (fBaseContext.IsGEDZIP() && mst == MediaStoreType.mstReference) {
                    mst = MediaStoreType.mstArchive;
                }

                string targetFn = this.FileName;

                switch (mst) {
                    case MediaStoreType.mstStorage:
                        fileName = fBaseContext.GetStgFolder(false) + targetFn;
                        break;

                    case MediaStoreType.mstArchive: {
                            fileName = GKUtils.GetTempDir() + Path.GetFileName(targetFn);
                            FileStream fs = new FileStream(fileName, FileMode.Create, FileAccess.Write);
                            try {
                                if (!File.Exists(fBaseContext.GetArcFileName())) {
                                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
                                } else {
                                    ArcFileLoad(fBaseContext, targetFn, fs);
                                }
                            } finally {
                                fs.Close();
                                fs.Dispose();
                            }
                        }
                        break;

                    case MediaStoreType.mstRelativeReference:
                        string treeName = fBaseContext.FileName;
                        fileName = fBaseContext.GetTreePath(treeName) + targetFn;
                        break;

                    case MediaStoreType.mstReference: {
                            fileName = targetFn;
                            if (!File.Exists(fileName)) {
                                string newPath = FileHelper.NormalizeFilename(fileName);
                                if (!string.IsNullOrEmpty(newPath)) {
                                    fileName = newPath;
                                }
                            }
                            break;
                        }

                    case MediaStoreType.mstURL: {
                            fileName = GKUtils.GetTempDir() + Path.GetFileName(targetFn);
                            FileStream fs = new FileStream(fileName, FileMode.Create, FileAccess.Write);
                            try {
                                var dataBytes = GKUtils.GetWebData(targetFn);
                                fs.Write(dataBytes, 0, dataBytes.Length);
                            } finally {
                                fs.Close();
                                fs.Dispose();
                            }
                        }
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.MediaLoad_fn()", ex);
                fileName = "";
            }

            return fileName;
        }

        public bool VerifyMediaFileWM()
        {
            string fileName;
            MediaStoreStatus storeStatus = VerifyMediaFile(out fileName);
            if (storeStatus != MediaStoreStatus.mssExists) {
                switch (storeStatus) {
                    case MediaStoreStatus.mssFileNotFound:
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileNotFound, fileName));
                        break;

                    case MediaStoreStatus.mssStgNotFound:
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.StgNotFound));
                        break;

                    case MediaStoreStatus.mssArcNotFound:
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.ArcNotFound));
                        break;

                    case MediaStoreStatus.mssBadData:
                        break;
                }
                return false;
            }
            return true;
        }

        public async Task<bool> MediaDelete()
        {
            try {
                string fileName = this.FileName;
                MediaStoreStatus storeStatus = VerifyMediaFile(out fileName);
                bool result = false;

                switch (storeStatus) {
                    case MediaStoreStatus.mssExists: {
                            if (this.StoreType == MediaStoreType.mstArchive || this.StoreType == MediaStoreType.mstStorage) {
                                if (!GlobalOptions.Instance.AllowDeleteMediaFileFromStgArc) {
                                    return true;
                                }
                            }

                            if (this.StoreType == MediaStoreType.mstReference || this.StoreType == MediaStoreType.mstRelativeReference) {
                                if (!GlobalOptions.Instance.AllowDeleteMediaFileFromRefs) {
                                    return true;
                                }
                            }

                            if (!GlobalOptions.Instance.DeleteMediaFileWithoutConfirm) {
                                string msg = string.Format(LangMan.LS(LSID.MediaFileDeleteQuery));
                                // TODO: may be Yes/No/Cancel?
                                var res = await AppHost.StdDialogs.ShowQuestion(msg);
                                if (!res) {
                                    return false;
                                }
                            }

                            if (this.StoreType == MediaStoreType.mstArchive) {
                                MediaStore.ArcFileDelete(fBaseContext, fileName);
                            } else {
                                File.Delete(fileName);
                            }
                            result = true;
                        }
                        break;

                    case MediaStoreStatus.mssFileNotFound:
                        result = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.ContinueQuestion, LangMan.LS(LSID.FileNotFound, fileName)));
                        break;

                    case MediaStoreStatus.mssStgNotFound:
                        result = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.ContinueQuestion, LangMan.LS(LSID.StgNotFound)));
                        break;

                    case MediaStoreStatus.mssArcNotFound:
                        result = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.ContinueQuestion, LangMan.LS(LSID.ArcNotFound)));
                        break;

                    case MediaStoreStatus.mssBadData:
                        // can be deleted
                        result = true;
                        break;
                }

                return result;
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.MediaDelete()", ex);
                return false;
            }
        }
    }
}
