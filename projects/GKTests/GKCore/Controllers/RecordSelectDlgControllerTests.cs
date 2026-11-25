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
    public class RecordSelectDlgControllerTests : ControllerTest
    {
        private IButton _btnSelect;
        private IButton _btnCreate;
        private IButton _btnCancel;
        private IComboBox _txtFastFilter;
        private IFilterControl _fltCtl;

        [Test]
        public void Test_RecordSelectDlgController()
        {
            IRecordSelectDialog view = CreateMockView();
            var controller = new RecordSelectDlgController(view);
            controller.InitView(fBaseWin);
            controller.RecType = GDMRecordType.rtIndividual;
        }

        private IRecordSelectDialog CreateMockView()
        {
            var view = Substitute.For<IRecordSelectDialog>();
            _btnSelect = SubstCtl<IButton>(view, "btnSelect");
            _btnCreate = SubstCtl<IButton>(view, "btnCreate");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _txtFastFilter = SubstCtl<IComboBox>(view, "txtFastFilter");
            _fltCtl = SubstCtl<IFilterControl>(view, "fltCtl");

            view.FilterText.Returns(Substitute.For<ITextBox>());
            view.RecordsList.Returns(Substitute.For<IListView>());
            return view;
        }
    }
}
