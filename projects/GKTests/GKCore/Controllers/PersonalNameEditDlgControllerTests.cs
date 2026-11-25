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
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITextBox _txtMarriedSurname;
        private ILabel _lblMarriedSurname;
        private IComboBox _cmbNameType;
        private ITextBox _txtNickname;
        private ITextBox _txtNameSuffix;
        private ITextBox _txtNamePrefix;
        private ITextBox _txtSurnamePrefix;
        private ITextBox _txtPatronymic;
        private ITextBox _txtName;
        private ITextBox _txtSurname;
        private IComboBox _cmbLanguage;

        [Test]
        public void Test_PersonalNameEditDlgController()
        {
            IPersonalNameEditDlg view = CreateMockView();
            var controller = new PersonalNameEditDlgController(view);
            controller.InitView(fBaseWin);

            var person = new GDMIndividualRecord(fBaseWin.Context.Tree);
            var personalName = new GDMPersonalName();

            controller.IndividualRecord = person;
            controller.PersonalName = personalName;

            _txtSurname.Text = "sample text";

            controller.Accept();

            Assert.AreEqual("sample text", personalName.Surname);

            controller.UpdateView();
        }

        private IPersonalNameEditDlg CreateMockView()
        {
            var view = Substitute.For<IPersonalNameEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            SubstCtl<ILabel>(view, "lblSurname");
            _lblMarriedSurname = SubstCtl<ILabel>(view, "lblMarriedSurname");
            SubstCtl<ILabel>(view, "lblName");
            SubstCtl<ILabel>(view, "lblPatronymic");
            SubstCtl<ILabel>(view, "lblNickname");
            SubstCtl<ILabel>(view, "lblSurnamePrefix");
            SubstCtl<ILabel>(view, "lblNamePrefix");
            SubstCtl<ILabel>(view, "lblNameSuffix");
            SubstCtl<ILabel>(view, "lblType");
            SubstCtl<ILabel>(view, "lblLanguage");

            _txtMarriedSurname = SubstCtl<ITextBox>(view, "txtMarriedSurname");
            _cmbNameType = SubstCtl<IComboBox>(view, "cmbNameType");
            _txtNickname = SubstCtl<ITextBox>(view, "txtNickname");
            _txtNameSuffix = SubstCtl<ITextBox>(view, "txtNameSuffix");
            _txtNamePrefix = SubstCtl<ITextBox>(view, "txtNamePrefix");
            _txtSurnamePrefix = SubstCtl<ITextBox>(view, "txtSurnamePrefix");
            _txtPatronymic = SubstCtl<ITextBox>(view, "txtPatronymic");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _txtSurname = SubstCtl<ITextBox>(view, "txtSurname");
            _cmbLanguage = SubstCtl<IComboBox>(view, "cmbLanguage");
            return view;
        }
    }
}
