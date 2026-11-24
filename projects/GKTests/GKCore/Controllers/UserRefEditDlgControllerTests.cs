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
    public class UserRefEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_UserRefEditDlgController()
        {
            IUserRefEditDlg view = CreateMockView();
            var controller = new UserRefEditDlgController(view);
            controller.InitView(null);
            controller.ApplyTheme();

            var userRef = new GDMUserReference();

            controller.UserReference = userRef;
            Assert.AreEqual(userRef, controller.UserReference);

            _cmbRef.Text = "sample text2";
            _cmbRefType.Text = "sample text3";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample text2", userRef.StringValue);
            Assert.AreEqual("sample text3", userRef.ReferenceType);
        }

        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblReference;
        private IComboBox _cmbRef;
        private ILabel _lblRefType;
        private IComboBox _cmbRefType;

        private IUserRefEditDlg CreateMockView()
        {
            var view = Substitute.For<IUserRefEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _lblReference = SubstCtl<ILabel>(view, "lblReference");
            _cmbRef = SubstCtl<IComboBox>(view, "cmbRef");
            _lblRefType = SubstCtl<ILabel>(view, "lblRefType");
            _cmbRefType = SubstCtl<IComboBox>(view, "cmbRefType");
            return view;
        }
    }
}
