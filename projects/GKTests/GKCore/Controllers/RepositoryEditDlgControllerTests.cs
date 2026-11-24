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
            controller.InitView(baseWin);

            var rep = baseWin.Context.Tree.CreateRepository();

            controller.RepositoryRecord = rep;
            Assert.AreEqual(rep, controller.RepositoryRecord);

            _txtName.Text = "test repo";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("test repo", rep.RepositoryName);

            // TODO: btnAddress, NotesList, UserRefList
        }

        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabControl _tabsData;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ITabPage _pageNotes;
        private IButton _btnAddress;
        private ISheetList _fNotesList;
        private ISheetList _fUserRefList;
        private ITabPage _pageUserRefs;

        private IRepositoryEditDlg CreateMockView()
        {
            var view = Substitute.For<IRepositoryEditDlg>();

            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _tabsData = SubstCtl<ITabControl>(view, "tabsData");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _btnAddress = SubstCtl<IButton>(view, "btnAddress");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fUserRefList = SubstCtl<ISheetList>(view, "fUserRefList");
            _pageUserRefs = SubstCtl<ITabPage>(view, "pageUserRefs");

            return view;
        }
    }
}
