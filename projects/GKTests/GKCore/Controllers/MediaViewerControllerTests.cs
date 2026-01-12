/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
