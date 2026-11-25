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
        private ITabPage _pageTreeMerge;
        private IButton _btnClose;
        private ILabel _lblMasterBase;
        private ITextBox _edMasterBase;
        private ILabel _lblOtherBase;
        private ITextBox _edUpdateBase;
        private IButton _btnTreeMerge;
        private ITextBox _mSyncRes;

        [Test]
        public void Test_TreeMergeController()
        {
            ITreeMergeDlg view = CreateMockView();
            var controller = new TreeMergeController(view);
            controller.InitView(fBaseWin);

            //SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            controller.Merge();

            controller.UpdateView();
        }

        private ITreeMergeDlg CreateMockView()
        {
            var view = Substitute.For<ITreeMergeDlg>();
            _pageTreeMerge = SubstCtl<ITabPage>(view, "pageTreeMerge");
            _btnClose = SubstCtl<IButton>(view, "btnClose");
            _lblMasterBase = SubstCtl<ILabel>(view, "lblMasterBase");
            _edMasterBase = SubstCtl<ITextBox>(view, "edMasterBase");
            _lblOtherBase = SubstCtl<ILabel>(view, "lblOtherBase");
            _edUpdateBase = SubstCtl<ITextBox>(view, "edUpdateBase");
            _btnTreeMerge = SubstCtl<IButton>(view, "btnTreeMerge");
            _mSyncRes = SubstCtl<ITextBox>(view, "mSyncRes");
            return view;
        }
    }
}
