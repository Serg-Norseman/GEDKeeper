/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using BSLib;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class LocExpertDlgControllerTests : ControllerTest
    {
        private IButton _btnClose;
        private ILabel _lblDate;
        private IButton _btnAnalysis;
        private ICheckBox _chkReverseOrder;
        private ITextBox _txtPlace;
        private ILabel _lblPlace;
        private ILabel _lblLocName;
        private IComboBox _cmbLocationSearch;
        private IButton _btnLocNameAdd;
        private IButton _btnLocNameEdit;
        private ILabel _lblTopLink;
        private IButton _btnTopLinkAdd;
        private IButton _btnTopLinkEdit;
        private ITextBox _txtGeneratedName;
        private ILabel _lblGeneratedName;
        private IComboBox _cmbEventDates;
        private ILabel _lblEventDates;
        private IDateControl _dtlPlaceDate;
        private IListView _lvEntries;

        [Test]
        public void Test_LocExpertDlgController()
        {
            ILocExpertDlg view = CreateMockView();
            var controller = new LocExpertDlgController(view);
            controller.InitView(fBaseWin);

            var eventDates = new SortedSet<GDMCustomDate>();
            controller.InitView(eventDates, "sample place");

            controller.Analyze();
            controller.UpdateView();

            var list = new StringList<GDMLocationRecord>();
            controller.GetLocationsList(list, "sample");
        }

        private ILocExpertDlg CreateMockView()
        {
            var view = Substitute.For<ILocExpertDlg>();
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _lblDate = SubstCtl<ILabel>(view, "lblDate");
            _btnAnalysis = SubstCtl<IButton>(view, "btnAnalysis");
            _chkReverseOrder = SubstCtl<ICheckBox>(view, "chkReverseOrder");
            _txtPlace = SubstCtl<ITextBox>(view, "txtPlace");
            _lblPlace = SubstCtl<ILabel>(view, "lblPlace");
            _lblLocName = SubstCtl<ILabel>(view, "lblLocName");
            _cmbLocationSearch = SubstCtl<IComboBox>(view, "cmbLocationSearch");
            _btnLocNameAdd = SubstCtl<IButton>(view, "btnLocNameAdd");
            _btnLocNameEdit = SubstCtl<IButton>(view, "btnLocNameEdit");
            _lblTopLink = SubstCtl<ILabel>(view, "lblTopLink");
            _btnTopLinkAdd = SubstCtl<IButton>(view, "btnTopLinkAdd");
            _btnTopLinkEdit = SubstCtl<IButton>(view, "btnTopLinkEdit");
            _txtGeneratedName = SubstCtl<ITextBox>(view, "txtGeneratedName");
            _lblGeneratedName = SubstCtl<ILabel>(view, "lblGeneratedName");
            _cmbEventDates = SubstCtl<IComboBox>(view, "cmbEventDates");
            _lblEventDates = SubstCtl<ILabel>(view, "lblEventDates");
            _dtlPlaceDate = SubstCtl<IDateControl>(view, "dtlPlaceDate");
            _lvEntries = SubstCtl<IListView>(view, "lvEntries");
            return view;
        }
    }
}
