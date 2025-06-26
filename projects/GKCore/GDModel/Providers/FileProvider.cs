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
using System.Text;

namespace GDModel.Providers
{
    /// <summary>
    /// Abstract class of generalized provider of files read / write operations.
    /// </summary>
    public abstract class FileProvider
    {
        protected readonly GDMTree fTree;


        protected FileProvider(GDMTree tree)
        {
            fTree = tree;
        }

        public abstract string GetFilesFilter();

        public void LoadFromString(string strText, bool charsetDetection = false)
        {
            using (MemoryStream stream = new MemoryStream(Encoding.UTF8.GetBytes(strText))) {
                LoadFromStreamExt(stream, stream, charsetDetection);
            }
        }

        public virtual void LoadFromFile(string fileName, bool charsetDetection = false)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                LoadFromStreamExt(fileStream, fileStream, charsetDetection);
            }
        }

        protected virtual Encoding GetDefaultEncoding(Stream inputStream)
        {
            return Encoding.UTF8;
        }

        public virtual void LoadFromStreamExt(Stream fileStream, Stream inputStream, bool charsetDetection = false)
        {
            fTree.Clear();
            ReadStream(fileStream, inputStream, charsetDetection);
        }

        protected abstract void ReadStream(Stream fileStream, Stream inputStream, bool charsetDetection = false);
    }
}
