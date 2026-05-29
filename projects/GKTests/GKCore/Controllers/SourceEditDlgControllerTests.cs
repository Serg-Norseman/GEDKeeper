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
            controller.Init(fBaseWin);

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

            view.ShortTitle.Text = "sample text";
            view.Author.Text.Returns("sample text");
            view.Date.Date = new GDMDate();

            controller.Accept();

            Assert.AreEqual("sample text", src.ShortTitle);
            Assert.AreEqual("sample text", src.Originator.Lines.Text);

            controller.UpdateView();
        }

        private static ISourceEditDlg CreateMockView()
        {
            var view = Substitute.For<ISourceEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");

            SubstituteControl<ILabel>(view, "lblShortTitle");
            SubstituteControl<ILabel>(view, "lblAuthor");
            SubstituteControl<ILabel>(view, "lblTitle");
            SubstituteControl<ILabel>(view, "lblPublication");
            SubstituteControl<ILabel>(view, "lblDate");

            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageText");
            SubstituteControl<ITabPage>(view, "pageRepositories");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ITabPage>(view, "pageUserRefs");

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            view.RepositoriesList.Returns(Substitute.For<ISheetList>());
            view.UserRefList.Returns(Substitute.For<ISheetList>());
            view.ShortTitle.Returns(Substitute.For<ITextBox>());
            view.Author.Returns(Substitute.For<ITextBox>());
            view.DescTitle.Returns(Substitute.For<ITextBox>());
            view.Publication.Returns(Substitute.For<ITextBox>());
            view.Text.Returns(Substitute.For<ITextBox>());
            view.Date.Returns(Substitute.For<IDateControl>());
            return view;
        }
    }
}
