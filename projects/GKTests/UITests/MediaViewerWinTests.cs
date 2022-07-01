/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

#if !MONO

using System.IO;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class MediaViewerWinTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private MediaViewerWin fDialog;
        private GDMMultimediaRecord fMediaRec;

        public override void Setup()
        {
            TestUtils.InitUITest();

            fBase = new BaseWindowStub();
            fMediaRec = fBase.Context.Tree.CreateMultimedia();

            fDialog = new MediaViewerWin(fBase);
            fDialog.MultimediaRecord = fMediaRec;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        private GDMFileReferenceWithTitle GetTestMultimedia(
            string resName, GDMMultimediaFormat multimediaFormat, out string targetName)
        {
            targetName = TestUtils.PrepareTestFile(resName);

            fMediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
            var fileRefV = fMediaRec.FileReferences[0];

            fileRefV.Title = "File Title";
            fileRefV.LinkFile(targetName);
            fileRefV.MediaType = GDMMediaType.mtManuscript;
            fileRefV.MultimediaFormat = multimediaFormat;
            
            return fileRefV;
        }

        [Test]
        public void Test_Image()
        {
            string targetName;
            var fileRefV = GetTestMultimedia("shaytan_plant.jpg", GDMMultimediaFormat.mfJPG, out targetName);
            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.FileReference = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.FileReference);

                fDialog.Refresh();

                ClickToolStripButton("btnZoomIn", fDialog);
                ClickToolStripButton("btnZoomOut", fDialog);
                ClickToolStripButton("btnSizeToFit", fDialog);

                KeyDownForm(fDialog.Name, Keys.Escape);
            } finally {
                TestUtils.RemoveTestFile(targetName);
            }
        }

        [Test]
        public void Test_Text()
        {
            string targetName;
            var fileRefV = GetTestMultimedia("lorem_ipsum.txt", GDMMultimediaFormat.mfTXT, out targetName);
            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.FileReference = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.FileReference);

                fDialog.Refresh();

                KeyDownForm(fDialog.Name, Keys.Escape);
            } finally {
                TestUtils.RemoveTestFile(targetName);
            }
        }

        [Test]
        public void Test_RTF()
        {
            string targetName;
            var fileRefV = GetTestMultimedia("lorem_ipsum.rtf", GDMMultimediaFormat.mfRTF, out targetName);
            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.FileReference = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.FileReference);

                fDialog.Refresh();

                KeyDownForm(fDialog.Name, Keys.Escape);
            } finally {
                TestUtils.RemoveTestFile(targetName);
            }
        }

        [Test]
        public void Test_HTML()
        {
            string targetName;
            var fileRefV = GetTestMultimedia("lorem_ipsum.htm", GDMMultimediaFormat.mfHTM, out targetName);
            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.FileReference = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.FileReference);

                fDialog.Refresh();

                KeyDownForm(fDialog.Name, Keys.Escape);
            } finally {
                TestUtils.RemoveTestFile(targetName);
            }
        }

        #if !CI_MODE
        [Test]
        public void Test_Video()
        {
            string targetName;
            var fileRefV = GetTestMultimedia("test_video.3gp", GDMMultimediaFormat.mfMKV, out targetName);

            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.FileReference = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.FileReference);

                fDialog.Refresh();

                ClickButton("btnPlay", fDialog);
                ClickButton("btnPause", fDialog);
                ClickButton("btnMute", fDialog);
                ClickButton("btnStop", fDialog);

                KeyDownForm(fDialog.Name, Keys.Escape);
            } finally {
                TestUtils.RemoveTestFile(targetName);
            }
        }
        #endif
    }
}

#endif
