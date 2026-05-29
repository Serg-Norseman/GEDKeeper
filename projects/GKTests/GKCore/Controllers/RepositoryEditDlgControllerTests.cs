/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class RepositoryEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_RepositoryEditDlgController()
        {
            IRepositoryEditDlg view = CreateMockView();
            var controller = new RepositoryEditDlgController(view);

            var baseWin = GetBaseSubst();
            controller.Init(baseWin);

            var rep = baseWin.Context.Tree.CreateRepository();

            controller.RepositoryRecord = rep;
            Assert.AreEqual(rep, controller.RepositoryRecord);

            view.Name.Text = "test repo";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("test repo", rep.RepositoryName);

            // TODO: btnAddress, NotesList, UserRefList
        }

        private static IRepositoryEditDlg CreateMockView()
        {
            var view = Substitute.For<IRepositoryEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageUserRefs");
            SubstituteControl<IButton>(view, "btnAddress");
            SubstituteControl<ILabel>(view, "lblName");

            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.UserRefList.Returns(Substitute.For<ISheetList>());
            view.Name.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
