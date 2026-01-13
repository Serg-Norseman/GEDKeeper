/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;

namespace GKCore.Media
{
    public sealed class URLMediaStore : MediaStore
    {
        public override MediaStoreType StoreType { get { return MediaStoreType.mstURL; } }


        public URLMediaStore(BaseContext baseContext) : base(baseContext)
        {
        }

        public URLMediaStore(BaseContext baseContext, string fileName) : base(baseContext, fileName)
        {
        }

        public override MediaStoreStatus VerifyMediaFile(out string displayFileName)
        {
            displayFileName = base.FileName;
            return MediaStoreStatus.mssExists;
        }

        protected override Stream LoadMediaStream(bool throwException)
        {
            string url = this.FileName;
            return GKUtils.GetWebStream(url);
        }

        protected override string LoadMediaFile()
        {
            string url = this.FileName;
            var dataBytes = GKUtils.GetWebData(url);

            string fileName = GKUtils.GetTempDir() + Path.GetFileName(url);
            using (var fs = new FileStream(fileName, FileMode.Create, FileAccess.Write)) {
                fs.Write(dataBytes, 0, dataBytes.Length);
                fs.Flush();
            }
            return fileName;
        }

        protected override void DeleteFile()
        {
            // not supported
        }
    }
}
