/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMFileReferenceTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMFileReference fileRef = new GDMFileReference()) {
                fileRef.MediaType = GDMMediaType.mtAudio;
                Assert.AreEqual(GDMMediaType.mtAudio, fileRef.MediaType);
            }

            Assert.AreEqual(GDMMultimediaFormat.mfUnknown, GDMFileReference.RecognizeFormat(""));
            Assert.AreEqual(GDMMultimediaFormat.mfUnknown, GDMFileReference.RecognizeFormat("sample.xxx"));
            Assert.AreEqual(GDMMultimediaFormat.mfBMP, GDMFileReference.RecognizeFormat("sample.BMP"));
            Assert.AreEqual(GDMMultimediaFormat.mfGIF, GDMFileReference.RecognizeFormat("sample.Gif"));
            Assert.AreEqual(GDMMultimediaFormat.mfJPG, GDMFileReference.RecognizeFormat("sample.jpg"));
            Assert.AreEqual(GDMMultimediaFormat.mfJPG, GDMFileReference.RecognizeFormat("sample.Jpeg"));
            Assert.AreEqual(GDMMultimediaFormat.mfOLE, GDMFileReference.RecognizeFormat("sample.ole"));
            Assert.AreEqual(GDMMultimediaFormat.mfPCX, GDMFileReference.RecognizeFormat("sample.pCx"));
            Assert.AreEqual(GDMMultimediaFormat.mfTIF, GDMFileReference.RecognizeFormat("sample.TiF"));
            Assert.AreEqual(GDMMultimediaFormat.mfTIF, GDMFileReference.RecognizeFormat("sample.tiff"));
            Assert.AreEqual(GDMMultimediaFormat.mfWAV, GDMFileReference.RecognizeFormat("sample.wav"));
            Assert.AreEqual(GDMMultimediaFormat.mfTXT, GDMFileReference.RecognizeFormat("sample.txt"));
            Assert.AreEqual(GDMMultimediaFormat.mfRTF, GDMFileReference.RecognizeFormat("sample.rtf"));
            Assert.AreEqual(GDMMultimediaFormat.mfAVI, GDMFileReference.RecognizeFormat("sample.AvI"));
            Assert.AreEqual(GDMMultimediaFormat.mfTGA, GDMFileReference.RecognizeFormat("sample.TGA"));
            Assert.AreEqual(GDMMultimediaFormat.mfPNG, GDMFileReference.RecognizeFormat("sample.png"));
            Assert.AreEqual(GDMMultimediaFormat.mfMPG, GDMFileReference.RecognizeFormat("sample.mpg"));
            Assert.AreEqual(GDMMultimediaFormat.mfMPG, GDMFileReference.RecognizeFormat("sample.mpeg"));
            Assert.AreEqual(GDMMultimediaFormat.mfHTM, GDMFileReference.RecognizeFormat("sample.htm"));
            Assert.AreEqual(GDMMultimediaFormat.mfHTM, GDMFileReference.RecognizeFormat("sample.html"));
        }

        [Test]
        public void Test_Assign1()
        {
            var instance = new GDMFileReference();
            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(null);
            });
        }

        [Test]
        public void Test_Assign2()
        {
            var instance = new GDMFileReferenceWithTitle();
            Assert.Throws(typeof(ArgumentException), () => {
                instance.Assign(null);
            });
        }

        [Test]
        public void Test_Clear()
        {
            var instance = new GDMFileReferenceWithTitle();
            instance.Clear();
        }
    }
}
