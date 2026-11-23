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
        [Test]
        public void Test_PersonsFilterDlgController()
        {
            IPersonsFilterDlg view = CreateMockView();
            var controller = new PersonsFilterDlgController(view, RecordsListModel<GDMRecord>.Create(fBaseWin.Context, GDMRecordType.rtIndividual, false));
            controller.InitView(fBaseWin);
            controller.UpdateView();

            controller.GetControl<IRadioButton>("rbAliveBefore").Checked = true;
            controller.GetControl<IRadioButton>("rbAll").Checked = true;

            controller.GetControl<IRadioButton>("rbSexMale").Checked = true;
            controller.GetControl<IRadioButton>("rbOnlyLive").Checked = true;

            controller.GetControl<IComboBox>("txtName").Text = "*Ivan*";

            controller.GetControl<IComboBox>("cmbResidence").Text = "*test place*";

            controller.GetControl<IComboBox>("cmbEventVal").Text = "*test event*";

            controller.GetControl<IComboBox>("cmbGroup").Text = "- any -";

            controller.GetControl<IComboBox>("cmbSource").Text = "- any -";

            controller.Accept();
        }

        private static IPersonsFilterDlg CreateMockView()
        {
            var view = Substitute.For<IPersonsFilterDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<IButton>(view, "btnReset");
            SubstituteControl<ITabPage>(view, "pageFieldsFilter");
            view.FilterGrid.Returns(Substitute.For<IFilterGridView>());

            SubstituteControl<ITabPage>(view, "pageSpecificFilter");
            SubstituteControl<IRadioButton>(view, "rbAll");
            SubstituteControl<IRadioButton>(view, "rbOnlyLive");
            SubstituteControl<IRadioButton>(view, "rbOnlyDead");
            SubstituteControl<IRadioButton>(view, "rbAliveBefore");
            SubstituteControl<IRadioButton>(view, "rbSexAll");
            SubstituteControl<IRadioButton>(view, "rbSexMale");
            SubstituteControl<IRadioButton>(view, "rbSexFemale");
            SubstituteControl<ILabel>(view, "lblNameMask");
            SubstituteControl<ILabel>(view, "lblPlaceMask");
            SubstituteControl<ILabel>(view, "lblEventsMask");
            SubstituteControl<ILabel>(view, "lblGroups");
            SubstituteControl<ILabel>(view, "lblSources");
            SubstituteControl<ICheckBox>(view, "chkOnlyPatriarchs");

            SubstituteControl<IRadioButton>(view, "rbAliveBefore");
            SubstituteControl<IRadioButton>(view, "rbAll");
            SubstituteControl<IRadioButton>(view, "rbSexMale");
            SubstituteControl<IRadioButton>(view, "rbOnlyLive");
            SubstituteControl<IComboBox>(view, "txtName");
            SubstituteControl<IComboBox>(view, "cmbResidence");
            SubstituteControl<IComboBox>(view, "cmbEventVal");
            SubstituteControl<IComboBox>(view, "cmbGroup");
            SubstituteControl<IComboBox>(view, "cmbSource");

            return view;
        }
    }
}
