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
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class CommunicationEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_CommunicationEditDlgController()
        {
            ICommunicationEditDlg view = CreateMockView();
            var controller = new CommunicationEditDlgController(view);
            controller.Init(fBaseWin);

            var commRec = fBaseWin.Context.Tree.CreateCommunication();
            controller.CommunicationRecord = commRec;
            Assert.AreEqual(commRec, controller.CommunicationRecord);

            view.Name.Text = "sample theme";
            controller.Accept();
            Assert.AreEqual("sample theme", commRec.CommName);

            view.Name.Text = "sample text";
            view.CorrType.SelectedIndex = 1;
            controller.Accept();
            Assert.AreEqual("sample text", commRec.CommName);
            Assert.AreEqual(GDMCommunicationType.ctEMail, commRec.CommunicationType);
            Assert.AreEqual("", commRec.Date.StringValue);

            view.Name.Text = "sample text";
            view.Date.NormalizeDate = "02.02.2000";
            controller.Accept();
            Assert.AreEqual("sample text", commRec.CommName);
            Assert.AreEqual("02 FEB 2000", commRec.Date.StringValue);

            var iRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(iRec1);
            controller.SetPerson();
            controller.Accept();

            controller.UpdateView();
        }

        private static ICommunicationEditDlg CreateMockView()
        {
            var view = Substitute.For<ICommunicationEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ILabel>(view, "lblTheme");
            SubstituteControl<ILabel>(view, "lblCorresponder");
            SubstituteControl<ILabel>(view, "lblType");
            SubstituteControl<ILabel>(view, "lblDate");

            view.Corresponder.Returns(Substitute.For<ITextBox>());
            view.CorrType.Returns(Substitute.For<IComboBox>());
            view.Date.Returns(Substitute.For<IDateBox>());
            view.Dir.Returns(Substitute.For<IComboBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            return view;
        }
    }
}
