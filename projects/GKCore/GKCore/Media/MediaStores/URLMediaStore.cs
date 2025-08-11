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

using System.IO;

namespace GKCore.Media
{
    public sealed class URLMediaStore : MediaStore
    {
        public override MediaStoreType StoreType { get { return MediaStoreType.mstURL; } }


        public URLMediaStore(IBaseContext baseContext) : base(baseContext)
        {
        }

        public URLMediaStore(IBaseContext baseContext, string fileName) : base(baseContext, fileName)
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
