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
    public class SourceCallNumberDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblCallNumber;
        private ITextBox _txtNumber;
        private ILabel _lblMediaType;
        private IComboBox _cmbMediaType;

        [Test]
        public void Test_SourceCallNumberDlgController()
        {
            ISourceCallNumberEditDlg view = CreateMockView();
            var controller = new SourceCallNumberDlgController(view);
            controller.InitView(fBaseWin);

            var callNumber = new GDMSourceCallNumber();
            controller.CallNumber = callNumber;
            Assert.AreEqual(callNumber, controller.CallNumber);

            _txtNumber.Text = "sample number";
            _cmbMediaType.SelectedIndex = (int)GDMMediaType.mtBook;

            controller.Accept();

            Assert.AreEqual("sample number", callNumber.StringValue);
            Assert.AreEqual(GDMMediaType.mtBook, callNumber.MediaType);

            controller.UpdateView();
        }

        private ISourceCallNumberEditDlg CreateMockView()
        {
            var view = Substitute.For<ISourceCallNumberEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _lblCallNumber = SubstCtl<ILabel>(view, "lblCallNumber");
            _txtNumber = SubstCtl<ITextBox>(view, "txtNumber");
            _lblMediaType = SubstCtl<ILabel>(view, "lblMediaType");
            _cmbMediaType = SubstCtl<IComboBox>(view, "cmbMediaType");
            return view;
        }
    }
}
