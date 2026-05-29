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
        [Test]
        public void Test_NameEditDlgController()
        {
            INameEditDlg view = CreateMockView();
            var controller = new NameEditDlgController(view);
            controller.Init(fBaseWin);

            var nameEntry = new NameEntry();
            controller.NameEntry = nameEntry;

            view.SexCombo.SelectedIndex = 1; // male
            view.Name.Text = "Ivan";
            view.FPatr.Text = "Ivanovna";
            view.MPatr.Text = "Ivanovich";

            controller.Accept();
            controller.UpdateView();

            Assert.AreEqual("Ivan", nameEntry.Name);
            Assert.AreEqual("Ivanovich", nameEntry.M_Patronymic);
            Assert.AreEqual("Ivanovna", nameEntry.F_Patronymic);
            Assert.AreEqual(GDMSex.svMale, nameEntry.Sex);
        }

        private static INameEditDlg CreateMockView()
        {
            var view = Substitute.For<INameEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblSex");
            SubstituteControl<IGroupBox>(view, "grpPatronymics");
            SubstituteControl<ILabel>(view, "lblFemale");
            SubstituteControl<ILabel>(view, "lblMale");

            view.Name.Returns(Substitute.For<ITextBox>());
            view.FPatr.Returns(Substitute.For<ITextBox>());
            view.MPatr.Returns(Substitute.For<ITextBox>());
            view.SexCombo.Returns(Substitute.For<IComboBox>());
            return view;
        }
    }
}
