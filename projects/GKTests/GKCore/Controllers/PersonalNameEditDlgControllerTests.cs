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
    public class PersonalNameEditDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_PersonalNameEditDlgController()
        {
            IPersonalNameEditDlg view = CreateMockView();
            var controller = new PersonalNameEditDlgController(view);
            controller.Init(fBaseWin);

            var person = new GDMIndividualRecord(fBaseWin.Context.Tree);
            var personalName = new GDMPersonalName();

            controller.IndividualRecord = person;
            controller.PersonalName = personalName;

            view.Surname.Text = "sample text";

            controller.Accept();

            Assert.AreEqual("sample text", personalName.Surname);

            controller.UpdateView();
        }

        private static IPersonalNameEditDlg CreateMockView()
        {
            var view = Substitute.For<IPersonalNameEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ILabel>(view, "lblSurname");
            SubstituteControl<ILabel>(view, "lblMarriedSurname");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblPatronymic");
            SubstituteControl<ILabel>(view, "lblNickname");
            SubstituteControl<ILabel>(view, "lblSurnamePrefix");
            SubstituteControl<ILabel>(view, "lblNamePrefix");
            SubstituteControl<ILabel>(view, "lblNameSuffix");
            SubstituteControl<ILabel>(view, "lblType");
            SubstituteControl<ILabel>(view, "lblLanguage");

            view.SurnameLabel.Returns(Substitute.For<ILabel>());
            view.Surname.Returns(Substitute.For<ITextBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.Patronymic.Returns(Substitute.For<ITextBox>());
            view.NameType.Returns(Substitute.For<IComboBox>());
            view.NamePrefix.Returns(Substitute.For<ITextBox>());
            view.Nickname.Returns(Substitute.For<ITextBox>());
            view.SurnamePrefix.Returns(Substitute.For<ITextBox>());
            view.NameSuffix.Returns(Substitute.For<ITextBox>());
            view.MarriedSurname.Returns(Substitute.For<ITextBox>());
            view.Language.Returns(Substitute.For<IComboBox>());
            return view;
        }
    }
}
