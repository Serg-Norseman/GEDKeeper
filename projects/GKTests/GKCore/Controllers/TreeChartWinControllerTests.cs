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
    public class TreeChartWinControllerTests : ControllerTest
    {
        [Test]
        public void Test_TreeChartWinController()
        {
            var view = CreateMockView();
            var controller = new TreeChartWinController(view);
        }

        private static ITreeChartWin CreateMockView()
        {
            var view = Substitute.For<ITreeChartWin>();

            SubstituteControl<IToolItem>(view, "tbGensAncestors");
            SubstituteControl<IToolItem>(view, "tbGensDescendants");
            SubstituteControl<IToolItem>(view, "tbModes");

            SubstituteControl<IMenuItem>(view, "miEdit");
            SubstituteControl<IMenuItem>(view, "miFatherAdd");
            SubstituteControl<IMenuItem>(view, "miMotherAdd");
            SubstituteControl<IMenuItem>(view, "miFamilyAdd");
            SubstituteControl<IMenuItem>(view, "miSpouseAdd");
            SubstituteControl<IMenuItem>(view, "miSonAdd");
            SubstituteControl<IMenuItem>(view, "miDaughterAdd");
            SubstituteControl<IMenuItem>(view, "miDelete");
            SubstituteControl<IMenuItem>(view, "miGoToRecord");
            SubstituteControl<IMenuItem>(view, "miGoToPrimaryBranch");
            SubstituteControl<IMenuItem>(view, "miOpenInNewWindow");
            SubstituteControl<IMenuItem>(view, "miMergeDuplicates");
            SubstituteControl<IMenuItem>(view, "miRebuildTree");
            SubstituteControl<IMenuItem>(view, "miRebuildKinships");
            SubstituteControl<IMenuItem>(view, "miSelectColor");

            SubstituteControl<IToolItem>(view, "tbImageSave");
            SubstituteControl<IToolItem>(view, "tbPrev");
            SubstituteControl<IToolItem>(view, "tbNext");
            SubstituteControl<IToolItem>(view, "tbOptions");
            SubstituteControl<IToolItem>(view, "tbFilter");

            SubstituteControl<IToolItem>(view, "tbGensCommon");
            SubstituteControl<IToolItem>(view, "tbBorders");
            SubstituteControl<IToolItem>(view, "tbDocPrint");
            SubstituteControl<IToolItem>(view, "tbDocPreview");
            SubstituteControl<IMenuItem>(view, "miFillImage");
            SubstituteControl<IMenuItem>(view, "miGensInfAncestors");
            SubstituteControl<IMenuItem>(view, "miGensInfDescendants");
            SubstituteControl<IMenuItem>(view, "miModeBoth");
            SubstituteControl<IMenuItem>(view, "miModeAncestors");
            SubstituteControl<IMenuItem>(view, "miModeDescendants");

            SubstituteControl<IMenuItem>(view, "miGensInfCommon");
            SubstituteControl<IMenuItem>(view, "miMaps");
            SubstituteControl<IMenuItem>(view, "miMapAncestors");
            SubstituteControl<IMenuItem>(view, "miMapDescendants");
            SubstituteControl<IMenuItem>(view, "miMapAll");

            SubstituteControl<IMenuItem>(view, "miFillColor");
            SubstituteControl<IMenuItem>(view, "miTraceSelected");
            SubstituteControl<IMenuItem>(view, "miTraceKinships");
            SubstituteControl<IMenuItem>(view, "miCertaintyIndex");
            SubstituteControl<IMenuItem>(view, "miXRefVisible");
            SubstituteControl<IMenuItem>(view, "miTrackSelectedLines");
            SubstituteControl<IMenuItem>(view, "miTrackMatchedSources");
            SubstituteControl<IMenuItem>(view, "miHideDescSpouses");
            SubstituteControl<IMenuItem>(view, "miParentAges");

            return view;
        }
    }
}
