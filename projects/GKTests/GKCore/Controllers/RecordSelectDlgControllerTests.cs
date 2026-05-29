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
        [Test]
        public void Test_RecordSelectDlgController()
        {
            IRecordSelectDialog view = CreateMockView();
            var controller = new RecordSelectDlgController(view);
            controller.Init(fBaseWin);
            controller.RecType = GDMRecordType.rtIndividual;
        }

        private static IRecordSelectDialog CreateMockView()
        {
            var view = Substitute.For<IRecordSelectDialog>();
            SubstituteControl<IButton>(view, "btnCreate");
            SubstituteControl<IButton>(view, "btnSelect");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITextBox>(view, "txtFastFilter");

            view.FilterCombo.Returns(Substitute.For<IComboBox>());
            view.FilterText.Returns(Substitute.For<ITextBox>());
            view.FilterCtl.Returns(Substitute.For<IFilterControl>());
            view.RecordsList.Returns(Substitute.For<IListView>());
            return view;
        }
    }
}
