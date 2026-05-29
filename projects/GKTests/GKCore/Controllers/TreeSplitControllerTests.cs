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
using GKCore.Tools;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class TreeSplitControllerTests : ControllerTest
    {

        [Test]
        public void Test_TreeSplitController()
        {
            ITreeSplitDlg view = CreateMockView();
            var controller = new TreeSplitController(view);
            controller.Init(fBaseWin);

            GDMIndividualRecord iRec = fBaseWin.Context.Tree.FindXRef<GDMIndividualRecord>("I1");
            Assert.IsNotNull(iRec);

            controller.UpdateView();

            controller.Select(iRec, TreeWalkMode.twmAll);

            // FIXME
            //ClickButton("btnSelectFamily", form);
            //ClickButton("btnSelectAncestors", form);
            //ClickButton("btnSelectDescendants", form);
            //ClickButton("btnSelectAll", form);

            /*SetModalFormHandler(fFormTest, SaveFile_Cancel_Handler);
            ClickButton("btnSave", form);

            try {
                SetModalFormHandler(fFormTest, SaveFileGED_Handler);
                ClickButton("btnSave", form);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.ged"));
            }*/

            // FIXME
            //ModalFormHandler = MessageBox_OkHandler;
            controller.Delete(); // <<- ui.ShowMessage()
        }

        private static ITreeSplitDlg CreateMockView()
        {
            var view = Substitute.For<ITreeSplitDlg>();
            SubstituteControl<ITabPage>(view, "pageTreeSplit");
            SubstituteControl<IButton>(view, "btnClose");
            SubstituteControl<IButton>(view, "btnSelectAll");
            SubstituteControl<IButton>(view, "btnSelectFamily");
            SubstituteControl<IButton>(view, "btnSelectAncestors");
            SubstituteControl<IButton>(view, "btnSelectDescendants");
            SubstituteControl<IButton>(view, "btnSelectList");
            SubstituteControl<IButton>(view, "btnDelete");
            SubstituteControl<IButton>(view, "btnSave");
            return view;
        }
    }
}
