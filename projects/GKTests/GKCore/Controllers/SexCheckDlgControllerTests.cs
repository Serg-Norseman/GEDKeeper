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
    public class SexCheckDlgControllerTests : ControllerTest
    {
        private ITextBox _txtName;
        private IGroupBox _grpSex;
        private IRadioButton _rbNone;
        private IRadioButton _rbMale;
        private IRadioButton _rbFemale;
        private IButton _btnAccept;
        private IButton _btnCancel;

        [Test]
        public void Test_SexCheckDlgController()
        {
            ISexCheckDlg view = CreateMockView();
            var controller = new SexCheckDlgController(view);
            controller.InitView(fBaseWin);

            //controller.indi = "test indi";
            controller.Sex = GDMSex.svMale;

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual(GDMSex.svMale, controller.Sex);
        }

        private ISexCheckDlg CreateMockView()
        {
            var view = Substitute.For<ISexCheckDlg>();
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _grpSex = SubstCtl<IGroupBox>(view, "grpSex");
            _rbNone = SubstCtl<IRadioButton>(view, "rbNone");
            _rbMale = SubstCtl<IRadioButton>(view, "rbMale");
            _rbFemale = SubstCtl<IRadioButton>(view, "rbFemale");
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            return view;
        }
    }
}
