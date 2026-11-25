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
        private IComboBox _cmbPersons;
        private ITreeView _tvPlaces;
        private IButton _btnSelectPlaces;
        private ICheckBox _chkBirth;
        private ICheckBox _chkDeath;
        private ICheckBox _chkResidence;
        private ICheckBox _chkLinesVisible;
        private IRadioButton _radTotal;
        private IRadioButton _radSelected;

        [Test]
        public void Test_MapsViewerWinController()
        {
            IMapsViewerWin view = CreateMockView();
            var controller = new MapsViewerWinController(view, null);
            controller.InitView(fBaseWin);

            controller.LoadPlaces();
            controller.SelectPlaces();
        }

        private IMapsViewerWin CreateMockView()
        {
            var view = Substitute.For<IMapsViewerWin>();
            SubstituteControl<ITabPage>(view, "pagePlaces");
            SubstituteControl<IGroupBox>(view, "grpSelection");
            SubstituteControl<ITabPage>(view, "pageCoordinates");

            view.MapBrowser.Returns(Substitute.For<IMapBrowser>());
            _cmbPersons = SubstCtl<IComboBox>(view, "cmbPersons");
            _tvPlaces = SubstCtl<ITreeView>(view, "tvPlaces");
            _btnSelectPlaces = SubstCtl<IButton>(view, "btnSelectPlaces");
            _chkBirth = SubstCtl<ICheckBox>(view, "chkBirth");
            _chkDeath = SubstCtl<ICheckBox>(view, "chkDeath");
            _chkResidence = SubstCtl<ICheckBox>(view, "chkResidence");
            _chkLinesVisible = SubstCtl<ICheckBox>(view, "chkLinesVisible");
            _radTotal = SubstCtl<IRadioButton>(view, "radTotal");
            _radSelected = SubstCtl<IRadioButton>(view, "radSelected");

            SubstituteControl<IToolItem>(view, "tbLoadPlaces");
            SubstituteControl<IToolItem>(view, "tbProviders");
            SubstituteControl<IToolItem>(view, "tbClear");
            SubstituteControl<IButton>(view, "btnSearch");
            SubstituteControl<IToolItem>(view, "tbSaveSnapshot");
            return view;
        }
    }
}
