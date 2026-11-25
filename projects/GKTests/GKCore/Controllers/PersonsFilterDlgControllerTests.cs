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
    public class PersonsFilterDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnReset;
        private ITabPage _pageFieldsFilter;
        private IFilterGridView _filterView;
        private ITabPage _pageSpecificFilter;
        private IComboBox _cmbEventVal;
        private IComboBox _cmbSource;
        private IComboBox _cmbGroup;
        private IComboBox _cmbResidence;
        private ITextBox _txtAliveBeforeDate;
        private IRadioButton _rbSexMale;
        private IRadioButton _rbSexAll;
        private IRadioButton _rbSexFemale;
        private IComboBox _txtName;
        private IRadioButton _rbAll;
        private IRadioButton _rbOnlyLive;
        private IRadioButton _rbOnlyDead;
        private IRadioButton _rbAliveBefore;
        private ICheckBox _chkOnlyPatriarchs;

        [Test]
        public void Test_PersonsFilterDlgController()
        {
            IPersonsFilterDlg view = CreateMockView();
            var controller = new PersonsFilterDlgController(view, RecordsListModel<GDMRecord>.Create(fBaseWin.Context, GDMRecordType.rtIndividual, false));
            controller.InitView(fBaseWin);
            controller.UpdateView();

            _rbAliveBefore.Checked = true;
            _rbAll.Checked = true;

            _rbSexMale.Checked = true;
            _rbOnlyLive.Checked = true;

            _txtName.Text = "*Ivan*";

            _cmbResidence.Text = "*test place*";

            _cmbEventVal.Text = "*test event*";

            _cmbGroup.Text = "- any -";

            _cmbSource.Text = "- any -";

            controller.Accept();
        }

        private IPersonsFilterDlg CreateMockView()
        {
            var view = Substitute.For<IPersonsFilterDlg>();

            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _btnReset = SubstCtl<IButton>(view, "btnReset");
            _pageFieldsFilter = SubstCtl<ITabPage>(view, "pageFieldsFilter");
            _filterView = SubstCtl<IFilterGridView>(view, "filterView");

            _pageSpecificFilter = SubstCtl<ITabPage>(view, "pageSpecificFilter");
            _rbAll = SubstCtl<IRadioButton>(view, "rbAll");
            _rbOnlyLive = SubstCtl<IRadioButton>(view, "rbOnlyLive");
            _rbOnlyDead = SubstCtl<IRadioButton>(view, "rbOnlyDead");
            _rbAliveBefore = SubstCtl<IRadioButton>(view, "rbAliveBefore");
            _rbSexAll = SubstCtl<IRadioButton>(view, "rbSexAll");
            _rbSexMale = SubstCtl<IRadioButton>(view, "rbSexMale");
            _rbSexFemale = SubstCtl<IRadioButton>(view, "rbSexFemale");
            SubstCtl<ILabel>(view, "lblNameMask");
            SubstCtl<ILabel>(view, "lblPlaceMask");
            SubstCtl<ILabel>(view, "lblEventsMask");
            SubstCtl<ILabel>(view, "lblGroups");
            SubstCtl<ILabel>(view, "lblSources");
            _chkOnlyPatriarchs = SubstCtl<ICheckBox>(view, "chkOnlyPatriarchs");

            _txtName = SubstCtl<IComboBox>(view, "txtName");
            _cmbResidence = SubstCtl<IComboBox>(view, "cmbResidence");
            _cmbEventVal = SubstCtl<IComboBox>(view, "cmbEventVal");
            _cmbGroup = SubstCtl<IComboBox>(view, "cmbGroup");
            _cmbSource = SubstCtl<IComboBox>(view, "cmbSource");

            SubstCtl<ITextBox>(view, "txtAliveBeforeDate");
            SubstCtl<IGroupBox>(view, "rgSex");
            SubstCtl<IGroupBox>(view, "rgLife");

            return view;
        }
    }
}
