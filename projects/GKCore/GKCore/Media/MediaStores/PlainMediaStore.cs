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
using BSLib;
using GKCore.Locales;

namespace GKCore.Media
{
    public abstract class PlainMediaStore : MediaStore
    {
        protected string fAbsoluteFileName;

        protected PlainMediaStore(IBaseContext baseContext, MediaStoreType storeType, string fileName) : base(baseContext, storeType, fileName)
        {
        }

        public override MediaStoreStatus VerifyMediaFile(out string displayFileName)
        {
            MediaStoreStatus result;

            try {
                displayFileName = LoadMediaFile();
                result = !File.Exists(displayFileName) ? MediaStoreStatus.mssFileNotFound : MediaStoreStatus.mssExists;
            } catch (Exception ex) {
                Logger.WriteError("PlainMediaStore.VerifyMediaFile()", ex);
                displayFileName = this.FileName;
                result = MediaStoreStatus.mssBadData;
            }

            return result;
        }

        protected override Stream LoadMediaStream(bool throwException)
        {
            string fileName = LoadMediaFile();

            Stream resultStream = null;

            if (!File.Exists(fileName)) {
                if (throwException) {
                    throw new MediaFileNotFoundException(fileName);
                }
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileNotFound, fileName));
            } else {
                resultStream = new FileStream(fileName, FileMode.Open, FileAccess.Read);
            }

            return resultStream;
        }

        protected override string LoadMediaFile()
        {
            string resultFileName = fAbsoluteFileName;

            if (!File.Exists(resultFileName)) {
                string newPath = FileHelper.NormalizeFilename(resultFileName);
                if (!string.IsNullOrEmpty(newPath) && File.Exists(newPath)) {
                    resultFileName = newPath;
                }
            }

            return resultFileName;
        }

        protected override void DeleteFile()
        {
            var fileName = LoadMediaFile();
            File.Delete(fileName);
        }
    }
}
