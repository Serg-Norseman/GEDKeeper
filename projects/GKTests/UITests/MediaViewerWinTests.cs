/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKTests;
using GKTests.Mocks;
using GKUI.Forms;
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
        private GEDCOMFileReferenceWithTitle fileRef;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();

            GEDCOMMultimediaRecord mmRec = fBase.Context.Tree.CreateMultimedia();
            mmRec.AddTag("FILE", "", null);
            fileRef = mmRec.FileReferences[0];
            fileRef.Title = "File Title 2";
            fileRef.LinkFile("shaytan_plant.jpg");
            fileRef.MediaType = GEDCOMMediaType.mtPhoto;

            fDialog = new MediaViewerWin(fBase);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            Assert.AreEqual(null, fDialog.FileRef);
            fDialog.FileRef = fileRef;
            Assert.AreEqual(fileRef, fDialog.FileRef);

            Assembly assembly = typeof(CoreTests).Assembly;
            Bitmap img = new Bitmap(assembly.GetManifestResourceStream("GKTests.Resources.shaytan_plant.jpg"));

            fDialog.SetViewImage(img, fileRef);

            fDialog.Refresh();

            ClickToolStripButton("btnZoomIn", fDialog);
            ClickToolStripButton("btnZoomOut", fDialog);
            ClickToolStripButton("btnSizeToFit", fDialog);

            KeyDownForm(fDialog.Name, Keys.Escape);
        }

        [Test]
        public void Test_Video()
        {
            string targetName = TestStubs.GetTempFilePath("test_video.3gp");

            Assembly assembly = typeof(CoreTests).Assembly;
            Stream vidstm = assembly.GetManifestResourceStream("GKTests.Resources.test_video.3gp");

            GKUtils.CopyFile(vidstm, new FileInfo(targetName), null);
            Assert.IsTrue(File.Exists(targetName));

            GEDCOMMultimediaRecord mmRecV = fBase.Context.Tree.CreateMultimedia();
            mmRecV.AddTag("FILE", "", null);
            var fileRefV = mmRecV.FileReferences[0];
            fileRefV.Title = "File Title 2";
            fileRefV.LinkFile(targetName);
            fileRefV.MediaType = GEDCOMMediaType.mtVideo;
            fileRefV.MultimediaFormat = GEDCOMMultimediaFormat.mfMKV;

            fDialog.FileRef = fileRefV;
            Assert.AreEqual(fileRefV, fDialog.FileRef);

            fDialog.SetViewMedia(targetName);
            fDialog.Refresh();

            ClickButton("btnPlay", fDialog);
            ClickButton("btnPause", fDialog);
            ClickButton("btnMute", fDialog);
            ClickButton("btnStop", fDialog);

            KeyDownForm(fDialog.Name, Keys.Escape);
        }
    }
}

#endif
