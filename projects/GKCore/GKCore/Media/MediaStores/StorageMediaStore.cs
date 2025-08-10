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
using GKCore.Locales;

namespace GKCore.Media
{
    public sealed class StorageMediaStore : PlainMediaStore
    {
        public StorageMediaStore(IBaseContext baseContext, MediaStoreType storeType, string fileName) : base(baseContext, storeType, fileName)
        {
        }

        public override MediaStoreStatus VerifyMediaFile(out string fileName)
        {
            MediaStoreStatus result = MediaStoreStatus.mssBadData;

            try {
                fileName = this.FileName;

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
            } catch (Exception ex) {
                Logger.WriteError("StorageMediaStore.VerifyMediaFile()", ex);
                fileName = string.Empty;
            }

            return result;
        }

        protected override Stream LoadMediaStream(bool throwException)
        {
            string targetFn = fBaseContext.GetStgFolder(false) + this.FileName;

            Stream resultStream = null;

            if (!File.Exists(targetFn)) {
                if (throwException) {
                    throw new MediaFileNotFoundException(targetFn);
                }
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
            } else {
                resultStream = new FileStream(targetFn, FileMode.Open, FileAccess.Read);
            }

            return resultStream;
        }

        protected override string LoadMediaFile()
        {
            string resultFileName = fBaseContext.GetStgFolder(false) + this.FileName;
            return resultFileName;
        }
    }
}
