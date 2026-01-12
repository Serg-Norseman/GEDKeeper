/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
