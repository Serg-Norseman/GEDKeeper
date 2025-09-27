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
using System.IO;
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
    public abstract class MediaStore
    {
        protected readonly BaseContext fBaseContext;

        public abstract MediaStoreType StoreType { get; }
        public string FileName { get; private set; }


        protected MediaStore(BaseContext baseContext)
        {
            fBaseContext = baseContext;
            FileName = string.Empty;
        }

        protected MediaStore(BaseContext baseContext, string fileName)
        {
            fBaseContext = baseContext;
            FileName = fileName;
        }


        public static MediaStoreType GetStoreType(string fileReference)
        {
            if (fileReference == null)
                throw new ArgumentNullException(nameof(fileReference));

            MediaStoreType result = MediaStoreType.mstReference;
            for (int i = 0; i < GKData.GKStoreTypes.Length; i++) {
                if (fileReference.StartsWith(GKData.GKStoreTypes[i].Sign, StringComparison.Ordinal)) {
                    result = (MediaStoreType)i;
                    break;
                }
            }
            return result;
        }

        public static MediaStoreType GetStoreType(string fileReference, out string filePath)
        {
            if (fileReference == null)
                throw new ArgumentNullException(nameof(fileReference));

            filePath = fileReference;

            MediaStoreType result;

            for (int i = 0; i < GKData.StoreDefinitions.Length; i++) {
                var storeDef = GKData.StoreDefinitions[i];

                var sign = storeDef.Sign;
                if (filePath.StartsWith(sign, StringComparison.Ordinal)) {
                    result = storeDef.StoreType;
                    if (result != MediaStoreType.mstURL) {
                        filePath = filePath.Remove(0, sign.Length);
                    }
                    return result;
                }
            }

            // no prefix signature (or URI scheme)
            result = MediaStoreType.mstReference_Old;
            return result;
        }

        public static string NormalizeRelativePath(string path)
        {
            if (string.IsNullOrEmpty(path))
                return path;

            path = path.TrimStart();
            char chrZ = path.Length > 0 ? path[0] : (char)0;

            // "." and ".." cases
            if (chrZ != '.') {
                path = (chrZ == '/') ? $".{path}" : $"./{path}";
            }

            return path;
        }

        public static MediaStore GetMediaStore(BaseContext baseContext, string fileReference)
        {
            if (string.IsNullOrEmpty(fileReference))
                throw new ArgumentNullException(nameof(fileReference));

            string filePath;
            MediaStoreType storeType = GetStoreType(fileReference, out filePath);

            /*if (storeType != MediaStoreType.mstReference && storeType != MediaStoreType.mstURL) {
                fileName = fileName.Remove(0, 4);
            }*/

            switch (storeType) {
                case MediaStoreType.mstReference:
                    // TODO: check for absolute/relative path and redirection to archive
                    // for relative paths from third-party programs (without prefix) is required
                    if (baseContext.IsGEDZIP()) {
                        return new ArchiveMediaStore(baseContext, filePath);
                    } else {
                        return new AbsolutePathMediaStore(baseContext, filePath);
                    }

                case MediaStoreType.mstRelativeReference:
                    return new RelativePathMediaStore(baseContext, filePath);

                /*case MediaStoreType.mstStorage:
                    return new StorageMediaStore(baseContext, fileName);*/

                case MediaStoreType.mstArchive:
                    return new ArchiveMediaStore(baseContext, filePath);

                case MediaStoreType.mstURL:
                    return new URLMediaStore(baseContext, filePath);

                default:
                    throw new NotSupportedException();
            }
        }

        public static MediaStore CreateMediaStore(BaseContext baseContext, MediaStoreType storeType)
        {
            switch (storeType) {
                case MediaStoreType.mstReference:
                    return new AbsolutePathMediaStore(baseContext);

                case MediaStoreType.mstRelativeReference:
                    return new RelativePathMediaStore(baseContext);

                /*case MediaStoreType.mstStorage:
                    return new StorageMediaStore(baseContext);*/

                case MediaStoreType.mstArchive:
                    return new ArchiveMediaStore(baseContext);

                case MediaStoreType.mstURL:
                    return new URLMediaStore(baseContext);

                default:
                    throw new NotSupportedException();
            }
        }


        public abstract MediaStoreStatus VerifyMediaFile(out string displayFileName);

        protected abstract Stream LoadMediaStream(bool throwException);

        public Stream MediaLoad(bool throwException)
        {
            return LoadMediaStream(throwException);
        }

        protected abstract string LoadMediaFile();

        public string MediaLoad()
        {
            string fileName;

            try {
                var mst = this.StoreType;
                if (mst != MediaStoreType.mstURL && !VerifyMediaFileWM()) {
                    return string.Empty;
                }

                return LoadMediaFile();
            } catch (Exception ex) {
                Logger.WriteError("MediaStore.MediaLoad_fn()", ex);
                fileName = string.Empty;
            }

            return fileName;
        }

        public bool VerifyMediaFileWM()
        {
            MediaStoreStatus storeStatus = VerifyMediaFile(out string displayFileName);
            if (storeStatus != MediaStoreStatus.mssExists) {
                switch (storeStatus) {
                    case MediaStoreStatus.mssFileNotFound:
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileNotFound, displayFileName));
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

        public bool MediaSave(string fileName, out string refPath)
        {
            string storeFile = Path.GetFileName(fileName);
            string storePath = GKUtils.GetStoreFolder(GKUtils.GetMultimediaKind(GDMFileReference.RecognizeFormat(fileName)));

            refPath = string.Empty;
            string targetFile = string.Empty;
            string refPrefix = GKData.GKStoreTypes[(int)StoreType].Sign;

            // set paths and links
            switch (StoreType) {
                case MediaStoreType.mstReference:
                    refPath = refPrefix + fileName;
                    break;

                case MediaStoreType.mstRelativeReference:
                    targetFile = fBaseContext.GetTreeRelativePath(fileName);
                    refPath = refPrefix + targetFile;
                    break;

                case MediaStoreType.mstArchive:
                    targetFile = storePath + storeFile;
                    refPath = refPrefix + targetFile;
                    break;

                /*case MediaStoreType.mstStorage:
                    targetFile = storePath + storeFile;
                    refPath = refPrefix + targetFile;
                    break;*/

                case MediaStoreType.mstURL:
                    refPath = fileName;
                    break;
            }

            if (StoreType != MediaStoreType.mstURL) {
                refPath = FileHelper.NormalizeFilename(refPath);
            }

            // verify existence
            if (fBaseContext.MediaExists(refPath)) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileWithSameNameAlreadyExists));
                return false;
            }

            // save a copy to archive or storage
            bool result = SaveCopy(fileName, targetFile);
            return result;
        }

        protected virtual bool SaveCopy(string sourceFileName, string targetFileName)
        {
            return true;
        }

        protected abstract void DeleteFile();

        public async Task<bool> MediaDelete()
        {
            try {
                MediaStoreStatus storeStatus = VerifyMediaFile(out string displayFileName);
                bool result = false;

                switch (storeStatus) {
                    case MediaStoreStatus.mssExists: {
                            if (this.StoreType == MediaStoreType.mstArchive /*|| this.StoreType == MediaStoreType.mstStorage*/) {
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

                            DeleteFile();
                            result = true;
                        }
                        break;

                    case MediaStoreStatus.mssFileNotFound:
                        result = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.ContinueQuestion, LangMan.LS(LSID.FileNotFound, displayFileName)));
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
                Logger.WriteError("MediaStore.MediaDelete()", ex);
                return false;
            }
        }
    }
}
