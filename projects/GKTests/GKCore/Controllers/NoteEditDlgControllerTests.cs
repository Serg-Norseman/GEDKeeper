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
            controller.Init(fBaseWin);

            var noteRecord = new GDMNoteRecord(fBaseWin.Context.Tree);
            controller.NoteRecord = noteRecord;

            view.Note.Text = "sample text";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample text", noteRecord.Lines.Text);
        }

        private static INoteEditDlg CreateMockView()
        {
            var view = Substitute.For<INoteEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");

            view.Note.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
