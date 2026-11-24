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
    public class FilePropertiesDlgControllerTests : ControllerTest
    {
        [Test]
        public void Test_FilePropertiesDlgController()
        {
            IFilePropertiesDlg view = CreateMockView();
            var controller = new FilePropertiesDlgController(view);

            var baseWin = GetBaseSubst();
            controller.InitView(baseWin);

            controller.UpdateView();

            _txtName.Text = "sample text";

            var langValue = GDMLanguageID.AngloSaxon;
            _txtLanguage.GetSelectedTag<GDMLanguageID>().Returns(langValue);

            controller.Accept();

            GDMSubmitterRecord submitter = baseWin.Context.Tree.GetPtrValue<GDMSubmitterRecord>(baseWin.Context.Tree.Header.Submitter);
            Assert.AreEqual("sample text", submitter.Name);
        }

        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageAuthor;
        private ILabel _lblName;
        private ILabel _lblAddress;
        private ILabel _lblTelephone;
        private ITextBox _txtName;
        private ITextBox _txtTel;
        private ITextBox _txtAddress;
        private ITabPage _pageOther;
        private IListView _lvRecordStats;
        private IButton _btnLangEdit;
        private IComboBox _txtLanguage;
        private ILabel _lblLanguage;

        private IFilePropertiesDlg CreateMockView()
        {
            var view = Substitute.For<IFilePropertiesDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _pageAuthor = SubstCtl<ITabPage>(view, "pageAuthor");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _lblAddress = SubstCtl<ILabel>(view, "lblAddress");
            _lblTelephone = SubstCtl<ILabel>(view, "lblTelephone");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _txtTel = SubstCtl<ITextBox>(view, "txtTel");
            _txtAddress = SubstCtl<ITextBox>(view, "txtAddress");
            _pageOther = SubstCtl<ITabPage>(view, "pageOther");
            _lvRecordStats = SubstCtl<IListView>(view, "lvRecordStats");
            _btnLangEdit = SubstCtl<IButton>(view, "btnLangEdit");
            _txtLanguage = SubstCtl<IComboBox>(view, "txtLanguage");
            _lblLanguage = SubstCtl<ILabel>(view, "lblLanguage");
            return view;
        }
    }
}
