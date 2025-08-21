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
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Options;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class MediaViewerControllerTests
    {
        private readonly IBaseWindow fBaseWin;

        public MediaViewerControllerTests()
        {
            TestUtils.InitUITest();

            LangMan.DefInit();
            GlobalOptions.Instance.AllowMediaStoreReferences = true;

            fBaseWin = new BaseWindowStub(true);
        }

        [Test]
        public void Test_MediaViewerController()
        {
            var view = Substitute.For<IMediaViewerWin>();
            var controller = new MediaViewerController(view);
            controller.Init(fBaseWin);

            controller.UpdateView();
        }

        private GDMMultimediaRecord GetTestMultimedia(string resName, out string targetName)
        {
            targetName = TestUtils.PrepareTestFile(resName);

            var mediaRec = fBaseWin.Context.Tree.CreateMultimedia();
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

                var view = Substitute.For<IMediaViewerWin>();
                var controller = new MediaViewerController(view);
                controller.Init(fBaseWin);
                controller.MultimediaRecord = fileRefV;
                controller.UpdateView();
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

                var view = Substitute.For<IMediaViewerWin>();
                var controller = new MediaViewerController(view);
                controller.Init(fBaseWin);
                controller.MultimediaRecord = fileRefV;
                controller.UpdateView();
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

                var view = Substitute.For<IMediaViewerWin>();
                var controller = new MediaViewerController(view);
                controller.Init(fBaseWin);
                controller.MultimediaRecord = fileRefV;
                controller.UpdateView();
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

                var view = Substitute.For<IMediaViewerWin>();
                var controller = new MediaViewerController(view);
                controller.Init(fBaseWin);
                controller.MultimediaRecord = fileRefV;
                controller.UpdateView();
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

                var view = Substitute.For<IMediaViewerWin>();
                var controller = new MediaViewerController(view);
                controller.Init(fBaseWin);
                controller.MultimediaRecord = fileRefV;
                controller.UpdateView();
            } finally {
                TestUtils.RemoveTestFile(targetName);
            }
        }
#endif
    }
}
