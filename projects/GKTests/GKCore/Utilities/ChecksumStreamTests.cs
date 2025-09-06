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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKTests;
using NUnit.Framework;

namespace GKCore.Utilities
{
    [TestFixture]
    public class ChecksumStreamTests
    {
        public ChecksumStreamTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_ChecksumLoadAndSave()
        {
            GEDCOMProvider.DebugWrite = true;
            using (Stream inStream = TestUtils.LoadResourceStream("test_loadsave.ged")) {
                using (GDMTree tree = new GDMTree()) {
                    byte[] inArray = TestUtils.stm2array(inStream);

                    var gedcomProvider = new GEDCOMProvider(tree, false, false);

                    var checksumStream = new ChecksumStream(inStream);
                    gedcomProvider.LoadFromStream(checksumStream);
                    ulong checksum = checksumStream.Checksum64;

                    using (MemoryStream outStream = new MemoryStream()) {
                        gedcomProvider = new GEDCOMProvider(tree);

                        var checksumStream2 = new ChecksumStream(outStream);
                        gedcomProvider.SaveToStreamExt(checksumStream2, GEDCOMCharacterSet.csUTF8);
                        ulong checksum2 = checksumStream2.Checksum64;

                        outStream.Position = 0;
                        byte[] outArray = outStream.ToArray();

                        Assert.AreEqual(inArray, outArray);

                        Assert.AreEqual(checksum, checksum2);
                    }
                }
            }
        }
    }
}
