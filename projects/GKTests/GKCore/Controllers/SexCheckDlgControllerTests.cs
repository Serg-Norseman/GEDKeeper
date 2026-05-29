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
        [Test]
        public void Test_SexCheckDlgController()
        {
            ISexCheckDlg view = CreateMockView();
            var controller = new SexCheckDlgController(view);
            controller.Init(fBaseWin);

            //controller.indi = "test indi";
            controller.Sex = GDMSex.svMale;

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual(GDMSex.svMale, controller.Sex);
        }

        private static ISexCheckDlg CreateMockView()
        {
            var view = Substitute.For<ISexCheckDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IGroupBox>(view, "grpSex");
            SubstituteControl<IRadioButton>(view, "rbNone");
            SubstituteControl<IRadioButton>(view, "rbMale");
            SubstituteControl<IRadioButton>(view, "rbFemale");
            SubstituteControl<ITextBox>(view, "txtName");
            return view;
        }
    }
}
