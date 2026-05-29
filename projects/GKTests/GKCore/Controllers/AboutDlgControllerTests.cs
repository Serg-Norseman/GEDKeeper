/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class AboutDlgControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_AboutDlgController()
        {
            IAboutDlg view = CreateMockView();
            var controller = new AboutDlgController(view);
            controller.SetLocale();
            controller.UpdateView();
        }

        private static IAboutDlg CreateMockView()
        {
            var view = Substitute.For<IAboutDlg>();
            SubstituteControl<ILabel>(view, "btnClose");
            SubstituteControl<ILabel>(view, "lblProduct");
            SubstituteControl<ILabel>(view, "lblVersion");
            SubstituteControl<ILabel>(view, "lblCopyright");
            SubstituteControl<ILabel>(view, "lblForum");
            SubstituteControl<ILabel>(view, "lblChannel");
            return view;
        }
    }
}
