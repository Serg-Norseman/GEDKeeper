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
    public class ParentsEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_ParentsEditDlgController()
        {
            IParentsEditDlg view = CreateMockView();
            var controller = new ParentsEditDlgController(view);
            controller.Init(fBaseWin);

            var individual = new GDMIndividualRecord(null);
            var childLink = new GDMChildToFamilyLink();

            controller.IndividualRecord = individual;
            controller.ChildLink = childLink;

            var relRec1 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec1);
            controller.AddFather();

            var relRec2 = fBaseWin.Context.Tree.CreateIndividual();
            RecordSelectDialogStub.SetTestResult(relRec2);
            controller.AddMother();

            controller.Accept();

            controller.UpdateView();
        }

        private static IParentsEditDlg CreateMockView()
        {
            var view = Substitute.For<IParentsEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblChildName");
            SubstituteControl<ILabel>(view, "lblParents");
            SubstituteControl<ILabel>(view, "lblLinkageType");
            SubstituteControl<IButton>(view, "btnParentsEdit");
            SubstituteControl<IButton>(view, "btnFatherAdd");
            SubstituteControl<IButton>(view, "btnFatherDelete");
            SubstituteControl<IButton>(view, "btnMotherAdd");
            SubstituteControl<IButton>(view, "btnMotherDelete");

            view.Father.Returns(Substitute.For<ITextBox>());
            view.Mother.Returns(Substitute.For<ITextBox>());
            view.ChildName.Returns(Substitute.For<ITextBox>());
            view.LinkageTypeCombo.Returns(Substitute.For<IComboBox>());
            return view;
        }
    }
}
