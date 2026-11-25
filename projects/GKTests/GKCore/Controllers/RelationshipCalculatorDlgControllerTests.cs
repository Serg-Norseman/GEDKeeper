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
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class RelationshipCalculatorDlgControllerTests : ControllerTest
    {
        private ILabel _lblKinship;
        private ITextBox _txtResult;
        private IButton _btnRec2Select;
        private IButton _btnRec1Select;
        private ITextBox _Edit2;
        private ITextBox _Edit1;
        private ILabel _Lab2;
        private ILabel _Lab1;
        private IButton _btnSwap;
        private IButton _btnClose;

        [Test]
        public void Test_RelationshipCalculatorDlgController()
        {
            IRelationshipCalculatorDlg view = CreateMockView();
            var controller = new RelationshipCalculatorDlgController(view);
            controller.InitView(fBaseWin);

            controller.SetRec1(null);
            controller.SetRec2(null);

            GDMIndividualRecord iRec1 = fBaseWin.Context.Tree.FindXRef<GDMIndividualRecord>("I1");
            Assert.IsNotNull(iRec1);
            Assert.AreEqual("Ivanov Ivan Ivanovich", GKUtils.GetRecordName(fBaseWin.Context.Tree, iRec1, false));
            GDMIndividualRecord iRec2 = fBaseWin.Context.Tree.FindXRef<GDMIndividualRecord>("I2");
            Assert.IsNotNull(iRec2);
            Assert.AreEqual("Ivanova Maria Petrovna", GKUtils.GetRecordName(fBaseWin.Context.Tree, iRec2, false));
            AppHost.TEST_MODE = true; // FIXME: dirty hack
            controller.SetRec1(iRec1);
            controller.SetRec2(iRec2);
            Assert.AreEqual("Ivanov Ivan Ivanovich is husband of Ivanova Maria Petrovna", _txtResult.Text); // :D

            var relRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec1);
            controller.SelectRec1();

            var relRec2 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec2);
            controller.SelectRec2();

            controller.UpdateView();

            controller.Swap();
        }

        private IRelationshipCalculatorDlg CreateMockView()
        {
            var view = Substitute.For<IRelationshipCalculatorDlg>();

            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _btnRec1Select = SubstCtl<IButton>(view, "btnRec1Select");
            _btnRec2Select = SubstCtl<IButton>(view, "btnRec2Select");
            _lblKinship = SubstCtl<ILabel>(view, "lblKinship");
            _btnSwap = SubstCtl<IButton>(view, "btnSwap");

            _Lab1 = SubstCtl<ILabel>(view, "Lab1");
            _Lab2 = SubstCtl<ILabel>(view, "Lab2");
            _Edit1 = SubstCtl<ITextBox>(view, "Edit1");
            _Edit2 = SubstCtl<ITextBox>(view, "Edit2");
            _txtResult = SubstCtl<ITextBox>(view, "txtResult");
            return view;
        }
    }
}
