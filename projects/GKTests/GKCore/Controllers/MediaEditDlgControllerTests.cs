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
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnView;
        private IButton _btnFileSelect;
        private ITabPage _pageNotes;
        private ITabPage _pageSources;
        private ITabPage _pageCommon;
        private ITabPage _pageFiles;
        private ITabPage _pageUserRefs;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblType;
        private IComboBox _cmbMediaType;
        private ILabel _lblStoreType;
        private IComboBox _cmbStoreType;
        private ILabel _lblFile;
        private ITextBox _txtFile;
        private ISheetList _fFilesList;
        private ISheetList _fNotesList;
        private ISheetList _fSourcesList;
        private ISheetList _fUserRefList;

        [Test]
        public async Task Test_MediaEditDlgController()
        {
            IMediaEditDlg view = CreateMockView();
            var controller = new MediaEditDlgController(view);
            controller.InitView(fBaseWin);

            var multimediaRecord = new GDMMultimediaRecord(fBaseWin.Context.Tree);
            multimediaRecord.FileReferences.Add(new GDMFileReferenceWithTitle());
            controller.MultimediaRecord = multimediaRecord;

            var mediaSampleFile = TestUtils.PrepareTestFile("shaytan_plant.jpg");
            try {
                _txtName.Text = "sample text";
                _cmbMediaType.SelectedIndex = 1;
                _cmbStoreType.SelectedIndex = 0; // Reference

                StdDialogsStub.SetOpenedFile(mediaSampleFile);
                await controller.SelectFile();

                controller.Accept();

                Assert.AreEqual("sample text", multimediaRecord.GetFileTitle());
            } finally {
                TestUtils.RemoveTestFile(mediaSampleFile);
            }
        }

        private IMediaEditDlg CreateMockView()
        {
            var view = Substitute.For<IMediaEditDlg>();

            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _btnView = SubstCtl<IButton>(view, "btnView");
            _btnFileSelect = SubstCtl<IButton>(view, "btnFileSelect");
            _pageCommon = SubstCtl<ITabPage>(view, "pageCommon");
            _pageFiles = SubstCtl<ITabPage>(view, "pageFiles");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageSources = SubstCtl<ITabPage>(view, "pageSources");
            _pageUserRefs = SubstCtl<ITabPage>(view, "pageUserRefs");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _lblType = SubstCtl<ILabel>(view, "lblType");
            _lblStoreType = SubstCtl<ILabel>(view, "lblStoreType");
            _lblFile = SubstCtl<ILabel>(view, "lblFile");

            _fFilesList = SubstCtl<ISheetList>(view, "fFilesList");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fSourcesList = SubstCtl<ISheetList>(view, "fSourcesList");
            _fUserRefList = SubstCtl<ISheetList>(view, "fUserRefList");
            _cmbMediaType = SubstCtl<IComboBox>(view, "cmbMediaType");
            _cmbStoreType = SubstCtl<IComboBox>(view, "cmbStoreType");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _txtFile = SubstCtl<ITextBox>(view, "txtFile");
            return view;
        }
    }
}
