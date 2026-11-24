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
            controller.InitView(fBaseWin);

            var commRec = fBaseWin.Context.Tree.CreateCommunication();
            controller.CommunicationRecord = commRec;
            Assert.AreEqual(commRec, controller.CommunicationRecord);

            _txtName.Text = "sample theme";
            controller.Accept();
            Assert.AreEqual("sample theme", commRec.CommName);

            _txtName.Text = "sample text";
            _cmbCorrType.SelectedIndex = 1;
            controller.Accept();
            Assert.AreEqual("sample text", commRec.CommName);
            Assert.AreEqual(GDMCommunicationType.ctEMail, commRec.CommunicationType);
            Assert.AreEqual("", commRec.Date.StringValue);

            _txtName.Text = "sample text";
            _txtDate.NormalizeDate = "02.02.2000";
            controller.Accept();
            Assert.AreEqual("sample text", commRec.CommName);
            Assert.AreEqual("02 FEB 2000", commRec.Date.StringValue);

            var iRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(iRec1);
            controller.SetPerson();
            controller.Accept();

            controller.UpdateView();
        }

        private IGroupBox _GroupBox1;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblTheme;
        private ITextBox _txtName;
        private ILabel _lblDate;
        private IDateBox _txtDate;
        private ILabel _lblType;
        private IComboBox _cmbCorrType;
        private IComboBox _txtDir;
        private ILabel _lblCorresponder;
        private ITextBox _txtCorresponder;
        private IButton _btnPersonAdd;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;

        private ICommunicationEditDlg CreateMockView()
        {
            var view = Substitute.For<ICommunicationEditDlg>();
            _GroupBox1 = SubstCtl<IGroupBox>(view, "GroupBox1");
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _lblTheme = SubstCtl<ILabel>(view, "lblTheme");
            _lblDate = SubstCtl<ILabel>(view, "lblDate");
            _lblType = SubstCtl<ILabel>(view, "lblType");
            _lblCorresponder = SubstCtl<ILabel>(view, "lblCorresponder");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _txtCorresponder = SubstCtl<ITextBox>(view, "txtCorresponder");
            _cmbCorrType = SubstCtl<IComboBox>(view, "cmbCorrType");
            _txtDir = SubstCtl<IComboBox>(view, "txtDir");
            _txtDate = SubstCtl<IDateBox>(view, "txtDate");
            _btnPersonAdd = SubstCtl<IButton>(view, "btnPersonAdd");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fMediaList = SubstCtl<ISheetList>(view, "fMediaList");
            return view;
        }
    }
}
