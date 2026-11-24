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
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class SourceEditDlgControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_SourceEditDlgController()
        {
            ISourceEditDlg view = CreateMockView();
            var controller = new SourceEditDlgController(view);
            controller.InitView(fBaseWin);

            var src = fBaseWin.Context.Tree.CreateSource();
            controller.SourceRecord = src;
            Assert.AreEqual(src, controller.SourceRecord);

            // add repository
            /*var repRec = fBaseWin.Context.Tree.CreateRepository();
            RecordSelectDialogStub.SetTestResult(repRec);
            await((RepositoryCitationsListModel)view.RepositoriesList.ListModel).Modify(view.RepositoriesList, new ModifyEventArgs(RecordAction.raAdd, null));
            Assert.AreEqual(1, src.RepositoryCitations.Count);
            // delete repository
            StdDialogsStub.SetQuestionResult(true);
            await((RepositoryCitationsListModel)view.RepositoriesList.ListModel).Modify(view.RepositoriesList, new ModifyEventArgs(RecordAction.raDelete, src.RepositoryCitations[0]));
            Assert.AreEqual(0, src.RepositoryCitations.Count);*/

            _txtShortTitle.Text = "sample text";
            _txtAuthor.Text.Returns("sample text");
            _dateCtl.Date = new GDMDate();

            controller.Accept();

            Assert.AreEqual("sample text", src.ShortTitle);
            Assert.AreEqual("sample text", src.Originator.Lines.Text);

            controller.UpdateView();
        }

        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabControl _tabsData;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageRepositories;
        private ITabPage _pageText;
        private ITextBox _txtText;
        private ITabPage _pageCommon;
        private ILabel _lblShortTitle;
        private ITextBox _txtShortTitle;
        private ILabel _lblAuthor;
        private ITextBox _txtAuthor;
        private ILabel _lblTitle;
        private ITextBox _txtTitle;
        private ILabel _lblPublication;
        private ITextBox _txtPublication;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
        private ISheetList _fRepositoriesList;
        private ILabel _lblDate;
        private IDateControl _dateCtl;
        private ITabPage _pageUserRefs;
        private ISheetList _fUserRefList;

        private ISourceEditDlg CreateMockView()
        {
            var view = Substitute.For<ISourceEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _tabsData = SubstCtl<ITabControl>(view, "tabsData");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _pageRepositories = SubstCtl<ITabPage>(view, "pageRepositories");
            _pageText = SubstCtl<ITabPage>(view, "pageText");
            _txtText = SubstCtl<ITextBox>(view, "txtText");
            _pageCommon = SubstCtl<ITabPage>(view, "pageCommon");
            _lblShortTitle = SubstCtl<ILabel>(view, "lblShortTitle");
            _txtShortTitle = SubstCtl<ITextBox>(view, "txtShortTitle");
            _lblAuthor = SubstCtl<ILabel>(view, "lblAuthor");
            _txtAuthor = SubstCtl<ITextBox>(view, "txtAuthor");
            _lblTitle = SubstCtl<ILabel>(view, "lblTitle");
            _txtTitle = SubstCtl<ITextBox>(view, "txtTitle");
            _lblPublication = SubstCtl<ILabel>(view, "lblPublication");
            _txtPublication = SubstCtl<ITextBox>(view, "txtPublication");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fMediaList = SubstCtl<ISheetList>(view, "fMediaList");
            _fRepositoriesList = SubstCtl<ISheetList>(view, "fRepositoriesList");
            _lblDate = SubstCtl<ILabel>(view, "lblDate");
            _dateCtl = SubstCtl<IDateControl>(view, "dateCtl");
            _pageUserRefs = SubstCtl<ITabPage>(view, "pageUserRefs");
            _fUserRefList = SubstCtl<ISheetList>(view, "fUserRefList");
            return view;
        }
    }
}
