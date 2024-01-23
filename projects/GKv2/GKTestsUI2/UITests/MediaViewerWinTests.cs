/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

#if !MONO && !DIS_NUF

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

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();

            fDialog = new MediaViewerWin(fBase);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        private GDMMultimediaRecord GetTestMultimedia(string resName, out string targetName)
        {
            targetName = TestUtils.PrepareTestFile(resName);

            var mediaRec = fBase.Context.Tree.CreateMultimedia();
            mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
            var fileRefV = mediaRec.FileReferences[0];

            fileRefV.Title = "File Title";
            fileRefV.LinkFile(targetName);
            fileRefV.MediaType = GDMMediaType.mtManuscript;

            return mediaRec;
        }

        [Test]
        public void Test_Image()
        {
            string targetName;
            var fileRefV = GetTestMultimedia("shaytan_plant.jpg", out targetName);
            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.MultimediaRecord = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.MultimediaRecord);

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
            var fileRefV = GetTestMultimedia("lorem_ipsum.txt", out targetName);
            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.MultimediaRecord = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.MultimediaRecord);

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
            var fileRefV = GetTestMultimedia("lorem_ipsum.rtf", out targetName);
            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.MultimediaRecord = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.MultimediaRecord);

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
            var fileRefV = GetTestMultimedia("lorem_ipsum.htm", out targetName);
            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.MultimediaRecord = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.MultimediaRecord);

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
            var fileRefV = GetTestMultimedia("test_video.3gp", out targetName);

            try {
                Assert.IsTrue(File.Exists(targetName));

                fDialog.MultimediaRecord = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.MultimediaRecord);

                fDialog.Refresh();

                //ClickButton("btnPlay", fDialog);
                //ClickButton("btnPause", fDialog);
                //ClickButton("btnMute", fDialog);
                //ClickButton("btnStop", fDialog);

                KeyDownForm(fDialog.Name, Keys.Escape);
            } finally {
                TestUtils.RemoveTestFile(targetName);
            }
        }
#endif
    }
}

#endif
