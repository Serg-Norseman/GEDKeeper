/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class NoteEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_NoteEditDlgController()
        {
            INoteEditDlg view = CreateMockView();
            var controller = new NoteEditDlgController(view);
            controller.InitView(fBaseWin);

            var noteRecord = new GDMNoteRecord(fBaseWin.Context.Tree);
            controller.NoteRecord = noteRecord;

            _txtNote.Text = "sample text";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample text", noteRecord.Lines.Text);
        }

        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITextBox _txtNote;
        private ITabControl _tabsData;
        private ISheetList _fSourcesList;
        private ISheetList _fUserRefList;

        private INoteEditDlg CreateMockView()
        {
            var view = Substitute.For<INoteEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _txtNote = SubstCtl<ITextBox>(view, "txtNote");
            _tabsData = SubstCtl<ITabControl>(view, "tabsData");
            _fSourcesList = SubstCtl<ISheetList>(view, "fSourcesList");
            _fUserRefList = SubstCtl<ISheetList>(view, "fUserRefList");
            return view;
        }
    }
}
