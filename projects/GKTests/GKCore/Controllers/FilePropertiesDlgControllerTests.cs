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
            controller.Init(baseWin);

            controller.UpdateView();

            view.Name.Text = "sample text";

            var langValue = GDMLanguageID.AngloSaxon;
            view.Language.GetSelectedTag<GDMLanguageID>().Returns(langValue);

            controller.Accept();

            GDMSubmitterRecord submitter = baseWin.Context.Tree.GetPtrValue<GDMSubmitterRecord>(baseWin.Context.Tree.Header.Submitter);
            Assert.AreEqual("sample text", submitter.Name);
        }

        private static IFilePropertiesDlg CreateMockView()
        {
            var view = Substitute.For<IFilePropertiesDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageAuthor");
            SubstituteControl<ITabPage>(view, "pageOther");
            SubstituteControl<ILabel>(view, "lblName");
            SubstituteControl<ILabel>(view, "lblAddress");
            SubstituteControl<ILabel>(view, "lblTelephone");
            SubstituteControl<ILabel>(view, "lblLanguage");

            view.RecordStats.Returns(Substitute.For<IListView>());
            view.Language.Returns(Substitute.For<IComboBox>());
            view.Name.Returns(Substitute.For<ITextBox>());
            view.Address.Returns(Substitute.For<ITextBox>());
            view.Tel.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
