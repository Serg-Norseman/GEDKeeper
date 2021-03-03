/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

#if !__MonoCS__

using System.Drawing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Forms;
using GKUI.Components;
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
        private GDMFileReferenceWithTitle fileRef;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowStub();

            GDMMultimediaRecord mmRec = fBase.Context.Tree.CreateMultimedia();
            mmRec.FileReferences.Add(new GDMFileReferenceWithTitle(mmRec));
            fileRef = mmRec.FileReferences[0];

            fileRef.Title = "File Title 2";
            fileRef.LinkFile("shaytan_plant.jpg");
            fileRef.MediaType = GDMMediaType.mtPhoto;

            fDialog = new MediaViewerWin(fBase);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Image()
        {
            Assert.AreEqual(null, fDialog.FileRef);
            fDialog.FileRef = fileRef;
            Assert.AreEqual(fileRef, fDialog.FileRef);

            Assembly assembly = typeof(CoreTests).Assembly;
            Bitmap img = new Bitmap(assembly.GetManifestResourceStream("GKTests.Resources.shaytan_plant.jpg"));
            IImage portableImage = new ImageHandler(img);

            fDialog.SetViewImage(portableImage, fileRef);
            fDialog.Refresh();

            ClickToolStripButton("btnZoomIn", fDialog);
            ClickToolStripButton("btnZoomOut", fDialog);
            ClickToolStripButton("btnSizeToFit", fDialog);

            KeyDownForm(fDialog.Name, Keys.Escape);
        }

        [Test]
        public void Test_Text()
        {
            Assert.AreEqual(null, fDialog.FileRef);
            fDialog.FileRef = fileRef;
            Assert.AreEqual(fileRef, fDialog.FileRef);

            Assembly assembly = typeof(CoreTests).Assembly;
            Stream stm = assembly.GetManifestResourceStream("GKTests.Resources.lorem_ipsum.txt");
            string text;
            using (StreamReader strd = new StreamReader(stm, Encoding.UTF8)) {
                text = strd.ReadToEnd();
            }

            fDialog.SetViewText(text);
            fDialog.Refresh();

            KeyDownForm(fDialog.Name, Keys.Escape);
        }

        [Test]
        public void Test_RTF()
        {
            Assert.AreEqual(null, fDialog.FileRef);
            fDialog.FileRef = fileRef;
            Assert.AreEqual(fileRef, fDialog.FileRef);

            Assembly assembly = typeof(CoreTests).Assembly;
            Stream stm = assembly.GetManifestResourceStream("GKTests.Resources.lorem_ipsum.txt");
            string text;
            using (StreamReader strd = new StreamReader(stm, Encoding.UTF8)) {
                text = strd.ReadToEnd();
            }

            fDialog.SetViewRTF(text);
            fDialog.Refresh();

            KeyDownForm(fDialog.Name, Keys.Escape);
        }

        [Test]
        public void Test_HTML()
        {
            Assert.AreEqual(null, fDialog.FileRef);
            fDialog.FileRef = fileRef;
            Assert.AreEqual(fileRef, fDialog.FileRef);

            Assembly assembly = typeof(CoreTests).Assembly;
            Stream stm = assembly.GetManifestResourceStream("GKTests.Resources.lorem_ipsum.txt");

            fDialog.SetViewHTML(stm);
            fDialog.Refresh();

            KeyDownForm(fDialog.Name, Keys.Escape);
        }

        #if !CI_MODE
        [Test]
        public void Test_Video()
        {
            string targetName = TestUtils.PrepareTestFile("test_video.3gp");

            try {
                Assert.IsTrue(File.Exists(targetName));

                GDMMultimediaRecord mmRecV = fBase.Context.Tree.CreateMultimedia();
                mmRecV.FileReferences.Add(new GDMFileReferenceWithTitle(mmRecV));
                var fileRefV = mmRecV.FileReferences[0];

                fileRefV.Title = "File Title 2";
                fileRefV.LinkFile(targetName);
                fileRefV.MediaType = GDMMediaType.mtVideo;
                fileRefV.MultimediaFormat = GDMMultimediaFormat.mfMKV;

                fDialog.FileRef = fileRefV;
                Assert.AreEqual(fileRefV, fDialog.FileRef);

                fDialog.SetViewMedia(targetName);
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
