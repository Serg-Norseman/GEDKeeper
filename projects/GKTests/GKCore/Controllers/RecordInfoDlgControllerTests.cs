/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class RecordInfoDlgControllerTests : ControllerTest
    {
        private IHyperView _hyperView1;

        [Test]
        public void Test_RecordInfoDlgController()
        {
            IRecordInfoDlg view = CreateMockView();
            _hyperView1.Lines.Returns(new StringList());

            var controller = new RecordInfoDlgController(view);
            controller.InitView(fBaseWin);

            var iRec = fBaseWin.Context.Tree.CreateIndividual();
            controller.Record = iRec;

            controller.UpdateView();
        }

        private IRecordInfoDlg CreateMockView()
        {
            var view = Substitute.For<IRecordInfoDlg>();
            _hyperView1 = SubstCtl<IHyperView>(view, "hyperView1");
            return view;
        }
    }
}
