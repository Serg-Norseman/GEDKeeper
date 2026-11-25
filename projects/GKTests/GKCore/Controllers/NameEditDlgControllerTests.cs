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
using GKCore.Names;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class NameEditDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblSex;
        private IComboBox _cmbSex;
        private IGroupBox _grpPatronymics;
        private ILabel _lblFemale;
        private ITextBox _txtFPatr;
        private ILabel _lblMale;
        private ITextBox _txtMPatr;

        [Test]
        public void Test_NameEditDlgController()
        {
            INameEditDlg view = CreateMockView();
            var controller = new NameEditDlgController(view);
            controller.InitView(fBaseWin);

            var nameEntry = new NameEntry();
            controller.NameEntry = nameEntry;

            _cmbSex.SelectedIndex = 1; // male
            _txtName.Text = "Ivan";
            _txtFPatr.Text = "Ivanovna";
            _txtMPatr.Text = "Ivanovich";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("Ivan", nameEntry.Name);
            Assert.AreEqual("Ivanovich", nameEntry.M_Patronymic);
            Assert.AreEqual("Ivanovna", nameEntry.F_Patronymic);
            Assert.AreEqual(GDMSex.svMale, nameEntry.Sex);
        }

        private INameEditDlg CreateMockView()
        {
            var view = Substitute.For<INameEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _lblSex = SubstCtl<ILabel>(view, "lblSex");
            _cmbSex = SubstCtl<IComboBox>(view, "cmbSex");
            _grpPatronymics = SubstCtl<IGroupBox>(view, "grpPatronymics");
            _lblFemale = SubstCtl<ILabel>(view, "lblFemale");
            _txtFPatr = SubstCtl<ITextBox>(view, "txtFPatr");
            _lblMale = SubstCtl<ILabel>(view, "lblMale");
            _txtMPatr = SubstCtl<ITextBox>(view, "txtMPatr");
            return view;
        }
    }
}
