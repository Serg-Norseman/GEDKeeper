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
    public class AgeEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_AgeEditDlgController()
        {
            IAgeEditDlg view = CreateMockView();
            var controller = new AgeEditDlgController(view);
            var baseWin = GetBaseSubst();
            controller.InitView(baseWin);

            var evt = new GDMIndividualEvent();
            controller.Event = evt;
            Assert.AreEqual(evt, controller.Event);

            controller.UpdateView();

            Assert.IsTrue(controller.Accept());
        }

        private IButton _btnAccept;
        private IButton _btnCancel;
        private IGroupBox _grpBox;
        private IComboBox _cmbRel1;
        private IComboBox _cmbRel2;
        private ILabel _lblAge1;
        private ITextBox _txtVal1;
        private ILabel _lblAge2;
        private ITextBox _txtVal2;

        private IAgeEditDlg CreateMockView()
        {
            var view = Substitute.For<IAgeEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _grpBox = SubstCtl<IGroupBox>(view, "grpBox");
            _cmbRel1 = SubstCtl<IComboBox>(view, "cmbRel1");
            _cmbRel2 = SubstCtl<IComboBox>(view, "cmbRel2");
            _lblAge1 = SubstCtl<ILabel>(view, "lblAge1");
            _txtVal1 = SubstCtl<ITextBox>(view, "txtVal1");
            _lblAge2 = SubstCtl<ILabel>(view, "lblAge2");
            _txtVal2 = SubstCtl<ITextBox>(view, "txtVal2");
            return view;
        }
    }
}
