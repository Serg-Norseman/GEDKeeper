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
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageCommon;
        private ITabPage _pageOther;
        private ILabel _lblSource;
        private IComboBox _cmbSource;
        private IButton _btnSourceAdd;
        private ILabel _lblPage;
        private ITextBox _txtPage;
        private ILabel _lblCertainty;
        private IComboBox _txtCertainty;
        private IDateControl _dateCtl;
        private ITextBox _txtText;

        [Test]
        public void Test_SourceCitEditDlgController()
        {
            ISourceCitEditDlg view = CreateMockView();
            var controller = new SourceCitEditDlgController(view);
            controller.InitView(fBaseWin);

            var sourceCitation = new GDMSourceCitation();
            controller.SourceCitation = sourceCitation;

            controller.Accept();

            controller.UpdateView();
        }

        private ISourceCitEditDlg CreateMockView()
        {
            var view = Substitute.For<ISourceCitEditDlg>();

            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _pageCommon = SubstCtl<ITabPage>(view, "pageCommon");
            _pageOther = SubstCtl<ITabPage>(view, "pageOther");
            _lblSource = SubstCtl<ILabel>(view, "lblSource");
            _cmbSource = SubstCtl<IComboBox>(view, "cmbSource");
            _btnSourceAdd = SubstCtl<IButton>(view, "btnSourceAdd");
            _lblPage = SubstCtl<ILabel>(view, "lblPage");
            _txtPage = SubstCtl<ITextBox>(view, "txtPage");
            _lblCertainty = SubstCtl<ILabel>(view, "lblCertainty");
            _txtCertainty = SubstCtl<IComboBox>(view, "txtCertainty");
            _dateCtl = SubstCtl<IDateControl>(view, "dateCtl");
            _txtText = SubstCtl<ITextBox>(view, "txtText");

            return view;
        }
    }
}
