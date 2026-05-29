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
            controller.Init(fBaseWin);
            controller.UpdateView();

            view.GetCoreControl<IRadioButton>("rbAliveBefore").Checked = true;
            view.GetCoreControl<IRadioButton>("rbAll").Checked = true;

            view.GetCoreControl<IRadioButton>("rbSexMale").Checked = true;
            view.GetCoreControl<IRadioButton>("rbOnlyLive").Checked = true;

            view.GetCoreControl<IComboBox>("txtName").Text = "*Ivan*";

            view.GetCoreControl<IComboBox>("cmbResidence").Text = "*test place*";

            view.GetCoreControl<IComboBox>("cmbEventVal").Text = "*test event*";

            view.GetCoreControl<IComboBox>("cmbGroup").Text = "- any -";

            view.GetCoreControl<IComboBox>("cmbSource").Text = "- any -";

            controller.Accept();
        }

        private static IPersonsFilterDlg CreateMockView()
        {
            var view = Substitute.For<IPersonsFilterDlg>();
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
            return view;
        }
    }
}
