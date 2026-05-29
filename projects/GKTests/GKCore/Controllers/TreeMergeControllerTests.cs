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
    public class TreeMergeControllerTests : ControllerTest
    {
        [Test]
        public void Test_TreeMergeController()
        {
            ITreeMergeDlg view = CreateMockView();
            var controller = new TreeMergeController(view);
            controller.Init(fBaseWin);

            //SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            controller.Merge();

            controller.UpdateView();
        }

        private static ITreeMergeDlg CreateMockView()
        {
            var view = Substitute.For<ITreeMergeDlg>();
            SubstituteControl<ITabPage>(view, "pageTreeMerge");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnTreeMerge");
            SubstituteControl<ILabel>(view, "lblMasterBase");
            SubstituteControl<ILabel>(view, "lblOtherBase");
            SubstituteControl<ITextBox>(view, "edMasterBase");

            view.UpdateBase.Returns(Substitute.For<ITextBox>());
            view.SyncLog.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
