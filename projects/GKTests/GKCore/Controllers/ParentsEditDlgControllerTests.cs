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
            controller.InitView(fBaseWin);

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

        private IButton _btnAccept;
        private IButton _btnCancel;
        private IGroupBox _GroupBox1;
        private ILabel _lblChildName;
        private ITextBox _txtChildName;
        private ILabel _lblLinkageType;
        private IComboBox _cmbLinkageType;
        private ILabel _lblParents;
        private ITextBox _txtFather;
        private ITextBox _txtMother;
        private IButton _btnParentsEdit;
        private IButton _btnFatherAdd;
        private IButton _btnFatherDelete;
        private IButton _btnMotherAdd;
        private IButton _btnMotherDelete;

        private IParentsEditDlg CreateMockView()
        {
            var view = Substitute.For<IParentsEditDlg>();

            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _GroupBox1 = SubstCtl<IGroupBox>(view, "GroupBox1");
            _lblChildName = SubstCtl<ILabel>(view, "lblChildName");
            _txtChildName = SubstCtl<ITextBox>(view, "txtChildName");
            _lblLinkageType = SubstCtl<ILabel>(view, "lblLinkageType");
            _cmbLinkageType = SubstCtl<IComboBox>(view, "cmbLinkageType");
            _lblParents = SubstCtl<ILabel>(view, "lblParents");
            _txtFather = SubstCtl<ITextBox>(view, "txtFather");
            _txtMother = SubstCtl<ITextBox>(view, "txtMother");
            _btnParentsEdit = SubstCtl<IButton>(view, "btnParentsEdit");
            _btnFatherAdd = SubstCtl<IButton>(view, "btnFatherAdd");
            _btnFatherDelete = SubstCtl<IButton>(view, "btnFatherDelete");
            _btnMotherAdd = SubstCtl<IButton>(view, "btnMotherAdd");
            _btnMotherDelete = SubstCtl<IButton>(view, "btnMotherDelete");

            return view;
        }
    }
}
