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
        public override MediaStoreType StoreType { get { return MediaStoreType.mstStorage; } }


        public StorageMediaStore(IBaseContext baseContext) : base(baseContext)
        {
        }

        public StorageMediaStore(IBaseContext baseContext, string fileName) : base(baseContext, fileName)
        {
            fAbsoluteFileName = baseContext.GetStgFolder() + fileName;
        }

        public override MediaStoreStatus VerifyMediaFile(out string displayFileName)
        {
            MediaStoreStatus result;

            try {
                string stgPath = fBaseContext.GetStgFolder();
                if (!Directory.Exists(stgPath)) {
                    displayFileName = this.FileName;
                    result = MediaStoreStatus.mssStgNotFound;
                } else {
                    displayFileName = fAbsoluteFileName;
                    result = !File.Exists(displayFileName) ? MediaStoreStatus.mssFileNotFound : MediaStoreStatus.mssExists;
                }
            } catch (Exception ex) {
                Logger.WriteError("StorageMediaStore.VerifyMediaFile()", ex);
                displayFileName = this.FileName;
                result = MediaStoreStatus.mssBadData;
            }

            return result;
        }

        protected override Stream LoadMediaStream(bool throwException)
        {
            Stream resultStream = null;

            if (!File.Exists(fAbsoluteFileName)) {
                if (throwException) {
                    throw new MediaFileNotFoundException(fAbsoluteFileName);
                }
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
            } else {
                resultStream = new FileStream(fAbsoluteFileName, FileMode.Open, FileAccess.Read);
            }

            return resultStream;
        }

        protected override string LoadMediaFile()
        {
            return fAbsoluteFileName;
        }

        protected override bool SaveCopy(string sourceFileName, string targetFileName)
        {
            bool result;
            targetFileName = fBaseContext.GetStgFolder() + targetFileName;
            try {
                string targetDir = Path.GetDirectoryName(targetFileName);
                if (!Directory.Exists(targetDir)) Directory.CreateDirectory(targetDir);

                result = fBaseContext.CopyFile(sourceFileName, targetFileName, !AppHost.TEST_MODE);
            } catch (IOException ex) {
                Logger.WriteError(string.Format("StorageMediaStore.SaveCopy({0}, {1})", sourceFileName, targetFileName), ex);
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileWithSameNameAlreadyExists));
                result = false;
            }
            return result;
        }
    }
}
