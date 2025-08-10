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
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Media
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class MediaStore
    {
        protected readonly IBaseContext fBaseContext;

        public MediaStoreType StoreType { get; private set; }
        public string FileName { get; private set; }


        protected MediaStore(IBaseContext baseContext, MediaStoreType storeType, string fileName)
        {
            fBaseContext = baseContext;
            StoreType = storeType;
            FileName = fileName;
        }


        public static MediaStoreType GetStoreType(string fileReference)
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

        public static MediaStore GetMediaStore(IBaseContext baseContext, string fileReference)
        {
            if (string.IsNullOrEmpty(fileReference))
                throw new ArgumentNullException(nameof(fileReference));

            string fileName = fileReference;
            MediaStoreType storeType = GetStoreType(fileName);

            if (storeType != MediaStoreType.mstReference && storeType != MediaStoreType.mstURL) {
                fileName = fileName.Remove(0, 4);
            }

            switch (storeType) {
                case MediaStoreType.mstReference:
                    // TODO: check for absolute/relative path and redirection to archive
                    // for relative paths from third-party programs (without prefix) is required
                    if (baseContext.IsGEDZIP()) {
                        return new ArchiveMediaStore(baseContext, storeType, fileName);
                    } else {
                        return new AbsolutePathMediaStore(baseContext, storeType, fileName);
                    }

                case MediaStoreType.mstRelativeReference:
                    return new RelativePathMediaStore(baseContext, storeType, fileName);

                case MediaStoreType.mstStorage:
                    return new StorageMediaStore(baseContext, storeType, fileName);

                case MediaStoreType.mstArchive:
                    return new ArchiveMediaStore(baseContext, storeType, fileName);

                case MediaStoreType.mstURL:
                    return new URLMediaStore(baseContext, storeType, fileName);

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

        protected abstract void DeleteFile();

        public async Task<bool> MediaDelete()
        {
            try {
                MediaStoreStatus storeStatus = VerifyMediaFile(out string displayFileName);
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
