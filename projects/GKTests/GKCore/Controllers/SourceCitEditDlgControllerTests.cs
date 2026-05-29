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
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class SourceCitEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_SourceCitEditDlgController()
        {
            ISourceCitEditDlg view = CreateMockView();
            var controller = new SourceCitEditDlgController(view);
            controller.Init(fBaseWin);

            var sourceCitation = new GDMSourceCitation();
            controller.SourceCitation = sourceCitation;

            controller.Accept();

            controller.UpdateView();
        }

        private static ISourceCitEditDlg CreateMockView()
        {
            var view = Substitute.For<ISourceCitEditDlg>();

            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ITabPage>(view, "pageOther");
            SubstituteControl<ILabel>(view, "lblSource");
            SubstituteControl<ILabel>(view, "lblPage");
            SubstituteControl<ILabel>(view, "lblCertainty");
            SubstituteControl<IButton>(view, "btnSourceAdd");

            view.Page.Returns(Substitute.For<ITextBox>());
            view.Certainty.Returns(Substitute.For<IComboBox>());
            view.Source.Returns(Substitute.For<IComboBox>());
            view.DataDate.Returns(Substitute.For<IDateControl>());
            view.DataText.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
