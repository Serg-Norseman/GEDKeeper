/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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

        protected PlainMediaStore(BaseContext baseContext) : base(baseContext)
        {
        }

        protected PlainMediaStore(BaseContext baseContext, string fileName) : base(baseContext, fileName)
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
