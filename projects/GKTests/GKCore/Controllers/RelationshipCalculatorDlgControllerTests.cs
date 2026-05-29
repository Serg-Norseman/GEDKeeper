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
        [Test]
        public void Test_RelationshipCalculatorDlgController()
        {
            IRelationshipCalculatorDlg view = CreateMockView();
            var controller = new RelationshipCalculatorDlgController(view);
            controller.Init(fBaseWin);

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
            Assert.AreEqual("Ivanov Ivan Ivanovich is husband of Ivanova Maria Petrovna", view.Result.Text); // :D

            var relRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec1);
            controller.SelectRec1();

            var relRec2 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec2);
            controller.SelectRec2();

            controller.UpdateView();

            controller.Swap();
        }

        private static IRelationshipCalculatorDlg CreateMockView()
        {
            var view = Substitute.For<IRelationshipCalculatorDlg>();

            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnRec1Select");
            SubstituteControl<IButton>(view, "btnRec2Select");
            SubstituteControl<ILabel>(view, "lblKinship");
            SubstituteControl<IButton>(view, "btnSwap");

            view.Label1.Returns(Substitute.For<ILabel>());
            view.Label2.Returns(Substitute.For<ILabel>());
            view.Person1.Returns(Substitute.For<ITextBox>());
            view.Person2.Returns(Substitute.For<ITextBox>());
            view.Result.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
