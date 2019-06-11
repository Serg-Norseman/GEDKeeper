/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;

namespace GDModel.Providers
{
    public delegate StackTuple AddTagHandler(GDMObject owner, int tagLevel, string tagName, string tagValue);

    public delegate bool SaveTagHandler(StreamWriter stream, int level, GDMTag tag);

    public sealed class StackTuple
    {
        public int Level;
        public GDMTag Tag;
        public AddTagHandler AddHandler;

        public StackTuple(int level, GDMTag tag)
        {
            Level = level;
            Tag = tag;
            AddHandler = null;
        }

        public StackTuple(int level, GDMTag tag, AddTagHandler addHandler)
        {
            Level = level;
            Tag = tag;
            if (addHandler != null) {
                AddHandler = addHandler;
            } else {
                AddHandler = GEDCOMFactory.GetInstance().GetAddHandler(tag.Name);
            }
        }
    }


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

        public void LoadFromFile(string fileName, bool charsetDetection = false)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                LoadFromStreamExt(fileStream, fileStream, charsetDetection);
            }
        }

        public void LoadFromStreamExt(Stream fileStream, Stream inputStream, bool charsetDetection = false)
        {
            using (StreamReader reader = FileHelper.OpenStreamReader(inputStream, GetDefaultEncoding())) {
                fTree.Clear();
                string streamCharset = DetectCharset(inputStream, charsetDetection);
                LoadFromReader(fileStream, reader, streamCharset);
            }
        }

        protected abstract Encoding GetDefaultEncoding();

        protected abstract string DetectCharset(Stream inputStream, bool charsetDetection);

        protected abstract void LoadFromReader(Stream fileStream, StreamReader reader, string streamCharset = null);
    }
}
