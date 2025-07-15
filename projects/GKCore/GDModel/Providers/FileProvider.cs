/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih, Alex Zaytsev.
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
using System.Text;

namespace GDModel.Providers
{
    /// <summary>
    /// Abstract class of generalized provider of files read / write operations.
    /// </summary>
    public abstract class FileProvider
    {
        protected readonly GDMTree fTree;

        private int fProgress;
        private long fStreamLength;
        private long fStreamPosition;


        protected FileProvider(GDMTree tree)
        {
            fTree = tree;
        }

        public abstract string GetFilesFilter();

        public void LoadFromString(string strText, bool charsetDetection = false)
        {
            using (var memStream = new MemoryStream(Encoding.UTF8.GetBytes(strText))) {
                InitProgress(memStream.Length);
                LoadFromStream(memStream, charsetDetection);
            }
        }

        public virtual void LoadFromFile(string fileName, bool charsetDetection = false)
        {
            using (var fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                InitProgress(fileStream.Length);
                LoadFromStream(fileStream, charsetDetection);
            }
        }

        public virtual void LoadFromStream(Stream inputStream, bool charsetDetection = false)
        {
            fTree.Clear();
            ReadStream(inputStream, charsetDetection);
        }

        protected abstract void ReadStream(Stream inputStream, bool charsetDetection = false);

        protected void InitProgress(long streamLength)
        {
            fProgress = 0;
            fStreamLength = streamLength;
        }

        protected void NotifyProgress(long streamPosition)
        {
            fStreamPosition = streamPosition;

            var progressCallback = fTree.ProgressCallback;
            if (progressCallback != null && fStreamLength != 0) {
                int newProgress = (int)(fStreamPosition * 100.0f / fStreamLength);
                if (newProgress <= 100 && fProgress != newProgress) {
                    fProgress = newProgress;
                    progressCallback.StepTo(fProgress);
                }
            }
        }
    }
}
