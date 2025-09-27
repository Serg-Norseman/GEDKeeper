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
using BSLib;
using GDModel;
using NUnit.Framework;

namespace GKCore.Media
{
    [TestFixture]
    public class MediaTests
    {
        [Test]
        public void Test_NamedRegion()
        {
            var region = new NamedRegion("test", ExtRect.Empty);
            Assert.AreEqual("test", region.Name);
        }

        [Test]
        public void Test_GetStoreType()
        {
            Assert.Throws(typeof(ArgumentNullException), () => {
                string strFileRef = null;
                MediaStore.GetStoreType(strFileRef);
            });

            var fileRef = new GDMFileReference();
            fileRef.ParseString("file:///file.txt");
            var storeType = MediaStore.GetStoreType(fileRef.StringValue);
            Assert.AreEqual(MediaStoreType.mstReference, storeType);

            /*fileRef.ParseString("stg:file.txt");
            storeType = MediaStore.GetStoreType(fileRef.StringValue);
            Assert.AreEqual(MediaStoreType.mstStorage, storeType);*/

            fileRef.ParseString("arcp:///file.txt");
            storeType = MediaStore.GetStoreType(fileRef.StringValue);
            Assert.AreEqual(MediaStoreType.mstArchive, storeType);

            fileRef.ParseString("file:./file.txt");
            storeType = MediaStore.GetStoreType(fileRef.StringValue);
            Assert.AreEqual(MediaStoreType.mstRelativeReference, storeType);
        }

        [Test]
        public void Test_GetStoreType_Ext()
        {
            Assert.Throws(typeof(ArgumentNullException), () => {
                string strFileRef = null;
                MediaStore.GetStoreType(strFileRef, out _);
            });

            string filePath;

            Assert.AreEqual(MediaStoreType.mstReference_Old, MediaStore.GetStoreType("file.txt", out filePath));
            Assert.AreEqual("file.txt", filePath);

            Assert.AreEqual(MediaStoreType.mstRelativeReference_Old, MediaStore.GetStoreType("rel:../file.txt", out filePath));
            Assert.AreEqual("../file.txt", filePath);

            Assert.AreEqual(MediaStoreType.mstStorage_Old, MediaStore.GetStoreType("stg:texts/file.txt", out filePath));
            Assert.AreEqual("texts/file.txt", filePath);

            Assert.AreEqual(MediaStoreType.mstArchive_Old, MediaStore.GetStoreType("arc:images/example.bmp", out filePath));
            Assert.AreEqual("images/example.bmp", filePath);


            Assert.AreEqual(MediaStoreType.mstReference, MediaStore.GetStoreType("file:///d:/my_family_tree/file.txt", out filePath));
            Assert.AreEqual("d:/my_family_tree/file.txt", filePath);

            Assert.AreEqual(MediaStoreType.mstRelativeReference, MediaStore.GetStoreType("file:./family_data/file.txt", out filePath));
            Assert.AreEqual("./family_data/file.txt", filePath);

            Assert.AreEqual(MediaStoreType.mstArchive, MediaStore.GetStoreType("arcp:///images/sample.png", out filePath));
            Assert.AreEqual("images/sample.png", filePath);


            Assert.AreEqual(MediaStoreType.mstURL, MediaStore.GetStoreType("http://www.ancestry.com/file.txt", out filePath));
            Assert.AreEqual("http://www.ancestry.com/file.txt", filePath);

            Assert.AreEqual(MediaStoreType.mstURL, MediaStore.GetStoreType("https://www.ancestry.com/file.txt", out filePath));
            Assert.AreEqual("https://www.ancestry.com/file.txt", filePath);
        }

        [Test]
        public void Test_NormalizeRelativePath()
        {
            Assert.AreEqual("./file.txt", MediaStore.NormalizeRelativePath("file.txt"));
            Assert.AreEqual("./file.txt", MediaStore.NormalizeRelativePath("/file.txt"));
            Assert.AreEqual("./file.txt", MediaStore.NormalizeRelativePath("./file.txt"));
            Assert.AreEqual("../file.txt", MediaStore.NormalizeRelativePath("../file.txt"));
        }
    }
}
