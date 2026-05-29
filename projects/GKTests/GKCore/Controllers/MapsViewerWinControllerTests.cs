/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class MapsViewerWinControllerTests : ControllerTest
    {
        [Test]
        public void Test_MapsViewerWinController()
        {
            IMapsViewerWin view = CreateMockView();
            var controller = new MapsViewerWinController(view, null);
            controller.Init(fBaseWin);
        }

        private static IMapsViewerWin CreateMockView()
        {
            var view = Substitute.For<IMapsViewerWin>();
            SubstituteControl<ITabPage>(view, "pagePlaces");
            SubstituteControl<IGroupBox>(view, "grpSelection");
            SubstituteControl<IRadioButton>(view, "radTotal");
            SubstituteControl<ICheckBox>(view, "chkBirth");
            SubstituteControl<ICheckBox>(view, "chkDeath");
            SubstituteControl<ICheckBox>(view, "chkResidence");
            SubstituteControl<IRadioButton>(view, "radSelected");
            SubstituteControl<IButton>(view, "btnSelectPlaces");
            SubstituteControl<ICheckBox>(view, "chkLinesVisible");
            SubstituteControl<IToolItem>(view, "tbLoadPlaces");
            SubstituteControl<IToolItem>(view, "tbProviders");
            SubstituteControl<ITabPage>(view, "pageCoordinates");
            SubstituteControl<IToolItem>(view, "tbClear");
            SubstituteControl<IButton>(view, "btnSearch");
            SubstituteControl<IToolItem>(view, "tbSaveSnapshot");

            view.MapBrowser.Returns(Substitute.For<IMapBrowser>());
            view.PersonsCombo.Returns(Substitute.For<IComboBox>());
            view.PlacesTree.Returns(Substitute.For<ITreeView>());
            view.SelectPlacesBtn.Returns(Substitute.For<IButton>());
            view.BirthCheck.Returns(Substitute.For<ICheckBox>());
            view.DeathCheck.Returns(Substitute.For<ICheckBox>());
            view.ResidenceCheck.Returns(Substitute.For<ICheckBox>());
            view.LinesVisibleCheck.Returns(Substitute.For<ICheckBox>());
            view.TotalRadio.Returns(Substitute.For<IRadioButton>());
            view.SelectedRadio.Returns(Substitute.For<IRadioButton>());
            return view;
        }
    }
}
