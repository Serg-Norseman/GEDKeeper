/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class MediaEditDlgControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_MediaEditDlgController()
        {
            IMediaEditDlg view = CreateMockView();
            var controller = new MediaEditDlgController(view);
            controller.Init(fBaseWin);

            var multimediaRecord = new GDMMultimediaRecord(fBaseWin.Context.Tree);
            multimediaRecord.FileReferences.Add(new GDMFileReferenceWithTitle());
            controller.MultimediaRecord = multimediaRecord;

            var mediaSampleFile = TestUtils.PrepareTestFile("shaytan_plant.jpg");
            try {
                view.Name.Text = "sample text";
                view.MediaType.SelectedIndex = 1;
                view.StoreType.SelectedIndex = 0; // Reference

                StdDialogsStub.SetOpenedFile(mediaSampleFile);
                await controller.SelectFile();

                controller.Accept();

                Assert.AreEqual("sample text", multimediaRecord.GetFileTitle());
            } finally {
                TestUtils.RemoveTestFile(mediaSampleFile);
            }
        }

        private static IMediaEditDlg CreateMockView()
        {
            var view = Substitute.For<IMediaEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageSources");
            SubstituteControl<ITabPage>(view, "pageUserRefs");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblType");
            SubstituteControl<ILabel>(view, "lblStoreType");
            SubstituteControl<ILabel>(view, "lblFile");
            SubstituteControl<IButton>(view, "btnView");

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.SourcesList.Returns(Substitute.For<ISheetList>());
            view.UserRefList.Returns(Substitute.For<ISheetList>());
            view.MediaType.Returns(Substitute.For<IComboBox>());
            view.StoreType.Returns(Substitute.For<IComboBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.File.Returns(Substitute.For<ITextBox>());
            view.FileSelectButton.Returns(Substitute.For<IButton>());
            return view;
        }
    }
}
