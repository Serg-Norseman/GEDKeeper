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
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnAddress;
        private IButton _btnPlaceAdd;
        private IButton _btnPlaceDelete;
        private IButton _btnAge;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageSources;
        private ITabPage _pageCommon;
        private IComboBox _cmbEventType;
        private ITextBox _txtEventPlace;
        private IComboBox _txtEventCause;
        private IComboBox _txtEventOrg;
        private IComboBox _txtAttribute;
        private IDateControl _dateCtl;
        private ITextBox _txtAge;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
        private ISheetList _fSourcesList;

        [Test]
        public void Test_EventEditDlgController()
        {
            IEventEditDlg view = CreateMockView();
            var controller = new EventEditDlgController(view);
            controller.InitView(fBaseWin);

            var evt = new GDMIndividualEvent();
            controller.Event = evt;

            _cmbEventType.SelectedIndex = 1; // Birth(indi) / ?(fam)
            _txtEventPlace.Text = "test place";

            // FIXME: create GKDateControl tests
            /*SelectCombo("cmbEventDateType", form, 3); // Between
            EnterMaskedText("txtEventDate1", form, "01.01.1900");
            EnterMaskedText("txtEventDate2", form, "10.01.1900");
            SelectCombo("cmbDate1Calendar", form, 1); // Julian
            SelectCombo("cmbDate2Calendar", form, 1); // Julian*/

            _txtEventCause.Text = "test cause";
            _txtEventOrg.Text = "test agency";

            //SetModalFormHandler(fFormTest, AddressEditDlgTests.AddressEditDlg_btnAccept_Handler);
            //ClickButton("btnAddress", form);

            var loc = fBaseWin.Context.Tree.CreateLocation();
            RecordSelectDialogStub.SetTestResult(loc);
            controller.AddPlace();

            controller.RemovePlace();

            controller.Accept();
            controller.UpdateView();
        }

        private IEventEditDlg CreateMockView()
        {
            var view = Substitute.For<IEventEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _btnAddress = SubstCtl<IButton>(view, "btnAddress");
            _btnPlaceAdd = SubstCtl<IButton>(view, "btnPlaceAdd");
            _btnPlaceDelete = SubstCtl<IButton>(view, "btnPlaceDelete");
            _pageCommon = SubstCtl<ITabPage>(view, "pageCommon");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _pageSources = SubstCtl<ITabPage>(view, "pageSources");
            SubstCtl<ILabel>(view, "lblEvent");
            SubstCtl<ILabel>(view, "lblAttrValue");
            SubstCtl<ILabel>(view, "lblPlace");
            SubstCtl<ILabel>(view, "lblDate");
            SubstCtl<ILabel>(view, "lblCause");
            SubstCtl<ILabel>(view, "lblOrg");
            SubstCtl<ILabel>(view, "lblAge");
            _txtAge = SubstCtl<ITextBox>(view, "txtAge");
            _btnAge = SubstCtl<IButton>(view, "btnAge");

            _cmbEventType = SubstCtl<IComboBox>(view, "cmbEventType");
            _txtEventPlace = SubstCtl<ITextBox>(view, "txtEventPlace");
            _txtEventCause = SubstCtl<IComboBox>(view, "txtEventCause");
            _txtEventOrg = SubstCtl<IComboBox>(view, "txtEventOrg");
            _txtAttribute = SubstCtl<IComboBox>(view, "txtAttribute");
            _dateCtl = SubstCtl<IDateControl>(view, "dateCtl");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fMediaList = SubstCtl<ISheetList>(view, "fMediaList");
            _fSourcesList = SubstCtl<ISheetList>(view, "fSourcesList");
            return view;
        }
    }
}
