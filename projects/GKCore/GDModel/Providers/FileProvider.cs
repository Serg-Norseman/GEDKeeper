/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih, Alex Zaytsev.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
