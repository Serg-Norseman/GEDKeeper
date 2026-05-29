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
using GKCore.Lists;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class EventEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_EventEditDlgController()
        {
            IEventEditDlg view = CreateMockView();
            var controller = new EventEditDlgController(view);
            controller.Init(fBaseWin);

            var evt = new GDMIndividualEvent();
            controller.Event = evt;

            view.EventType.SelectedIndex = 1; // Birth(indi) / ?(fam)
            view.Place.Text = "test place";

            // FIXME: create GKDateControl tests
            /*SelectCombo("cmbEventDateType", form, 3); // Between
            EnterMaskedText("txtEventDate1", form, "01.01.1900");
            EnterMaskedText("txtEventDate2", form, "10.01.1900");
            SelectCombo("cmbDate1Calendar", form, 1); // Julian
            SelectCombo("cmbDate2Calendar", form, 1); // Julian*/

            view.Cause.Text = "test cause";
            view.Agency.Text = "test agency";

            //SetModalFormHandler(fFormTest, AddressEditDlgTests.AddressEditDlg_btnAccept_Handler);
            //ClickButton("btnAddress", form);

            var loc = fBaseWin.Context.Tree.CreateLocation();
            RecordSelectDialogStub.SetTestResult(loc);
            controller.AddPlace();

            controller.RemovePlace();

            controller.Accept();
            controller.UpdateView();
        }

        private static IEventEditDlg CreateMockView()
        {
            var view = Substitute.For<IEventEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IButton>(view, "btnAddress");
            SubstituteControl<IButton>(view, "btnPlaceAdd");
            SubstituteControl<IButton>(view, "btnPlaceDelete");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageNotes");
            SubstituteControl<ITabPage>(view, "pageMultimedia");
            SubstituteControl<ITabPage>(view, "pageSources");
            SubstituteControl<ILabel>(view, "lblEvent");
            SubstituteControl<ILabel>(view, "lblAttrValue");
            SubstituteControl<ILabel>(view, "lblPlace");
            SubstituteControl<ILabel>(view, "lblDate");
            SubstituteControl<ILabel>(view, "lblCause");
            SubstituteControl<ILabel>(view, "lblOrg");
            SubstituteControl<ILabel>(view, "lblAge");
            SubstituteControl<ITextBox>(view, "txtAge");
            SubstituteControl<IButton>(view, "btnAge");

            view.EventType.Returns(Substitute.For<IComboBox>());
            view.Date.Returns(Substitute.For<IDateControl>());
            view.Attribute.Returns(Substitute.For<IComboBox>());
            view.Place.Returns(Substitute.For<ITextBox>());
            view.Cause.Returns(Substitute.For<IComboBox>());
            view.Agency.Returns(Substitute.For<IComboBox>());
            view.NotesList.Returns(Substitute.For<ISheetList>());
            view.MediaList.Returns(Substitute.For<ISheetList>());
            view.SourcesList.Returns(Substitute.For<ISheetList>());
            return view;
        }
    }
}
