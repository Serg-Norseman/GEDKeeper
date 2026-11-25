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
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class DNATestEditDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageCommon;
        private ILabel _lblTestName;
        private ITextBox _txtTestName;
        private ILabel _lblAgency;
        private IComboBox _cmbAgency;
        private ILabel _lblDate;
        private IDateBox _dateCtl;
        private ILabel _lblFileRef;
        private ITextBox _txtFileRef;
        private ILabel _lblStoreType;
        private IComboBox _cmbStoreType;
        private IButton _btnFileSelect;
        private ILabel _lblFileFormat;
        private IComboBox _cmbFileFormat;
        private ILabel _lblYHaplogroup;
        private IComboBox _cmbYHaplogroup;
        private ILabel _lblMHaplogroup;
        private IComboBox _cmbMHaplogroup;
        private ILabel _lblRestriction;
        private IComboBox _cmbRestriction;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;

        [Test]
        public void Test_DNATestEditDlgController()
        {
            IDNATestEditDlg view = CreateMockView();
            var controller = new DNATestEditDlgController(view);
            controller.InitView(fBaseWin);

            var dnaTest = new GDMDNATest();
            controller.DNATest = dnaTest;

            controller.UpdateView();
            controller.UpdateView();
        }

        private IDNATestEditDlg CreateMockView()
        {
            var view = Substitute.For<IDNATestEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _pageNotes = SubstCtl<ITabPage>(view, "pageNotes");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _pageCommon = SubstCtl<ITabPage>(view, "pageCommon");
            _lblTestName = SubstCtl<ILabel>(view, "lblTestName");
            _txtTestName = SubstCtl<ITextBox>(view, "txtTestName");
            _lblAgency = SubstCtl<ILabel>(view, "lblAgency");
            _cmbAgency = SubstCtl<IComboBox>(view, "cmbAgency");
            _lblDate = SubstCtl<ILabel>(view, "lblDate");
            _dateCtl = SubstCtl<IDateBox>(view, "dateCtl");
            _lblFileRef = SubstCtl<ILabel>(view, "lblFileRef");
            _txtFileRef = SubstCtl<ITextBox>(view, "txtFileRef");
            _lblStoreType = SubstCtl<ILabel>(view, "lblStoreType");
            _cmbStoreType = SubstCtl<IComboBox>(view, "cmbStoreType");
            _btnFileSelect = SubstCtl<IButton>(view, "btnFileSelect");
            _lblFileFormat = SubstCtl<ILabel>(view, "lblFileFormat");
            _cmbFileFormat = SubstCtl<IComboBox>(view, "cmbFileFormat");
            _lblYHaplogroup = SubstCtl<ILabel>(view, "lblYHaplogroup");
            _cmbYHaplogroup = SubstCtl<IComboBox>(view, "cmbYHaplogroup");
            _lblMHaplogroup = SubstCtl<ILabel>(view, "lblMHaplogroup");
            _cmbMHaplogroup = SubstCtl<IComboBox>(view, "cmbMHaplogroup");
            _lblRestriction = SubstCtl<ILabel>(view, "lblRestriction");
            _cmbRestriction = SubstCtl<IComboBox>(view, "cmbRestriction");
            _fNotesList = SubstCtl<ISheetList>(view, "fNotesList");
            _fMediaList = SubstCtl<ISheetList>(view, "fMediaList");
            return view;
        }
    }
}
