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
            controller.ApplyTheme();

            var userRef = new GDMUserReference();

            controller.UserReference = userRef;
            Assert.AreEqual(userRef, controller.UserReference);

            view.Ref.Text = "sample text2";
            view.RefType.Text = "sample text3";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("sample text2", userRef.StringValue);
            Assert.AreEqual("sample text3", userRef.ReferenceType);
        }

        private static IUserRefEditDlg CreateMockView()
        {
            var view = Substitute.For<IUserRefEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblReference");
            SubstituteControl<IComboBox>(view, "cmbRef");
            SubstituteControl<ILabel>(view, "lblRefType");
            SubstituteControl<IComboBox>(view, "cmbRefType");
            return view;
        }
    }
}
