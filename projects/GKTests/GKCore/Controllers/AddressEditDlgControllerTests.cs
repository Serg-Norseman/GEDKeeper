/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class AddressEditDlgControllerTests : ControllerTest
    {
        [Test]
        public async Task Test_AddressEditDlgController()
        {
            IAddressEditDlg view = CreateMockView();
            var controller = new AddressEditDlgController(view);
            controller.ApplyTheme();

            var addr = new GDMAddress();
            controller.Address = addr;
            Assert.AreEqual(addr, controller.Address);

            // fields
            addr.AddressCountry = "sample country";
            addr.AddressState = "state";
            addr.AddressCity = "city";
            addr.AddressPostalCode = "0123456";
            controller.UpdateView();
            AssertViewMatchesModel(addr, view);

            await TestPhonesActions(controller, addr);
            await TestMailsActions(controller, addr);
            await TestWebActions(controller, addr);

            // accept?
            controller.Accept();
            Assert.AreEqual("sample country", addr.AddressCountry);
        }

        private static async Task TestWebActions(AddressEditDlgController controller, GDMAddress addr)
        {
            StdDialogsStub.SetStrInputResult("test site");
            await controller.DoWebsAction(RecordAction.raAdd, null);
            Assert.AreEqual(1, addr.WebPages.Count);
            Assert.AreEqual("test site", addr.WebPages[0].StringValue);

            StdDialogsStub.SetStrInputResult("test site 2");
            await controller.DoWebsAction(RecordAction.raEdit, addr.WebPages[0]);
            Assert.AreEqual(1, addr.WebPages.Count);
            Assert.AreEqual("test site 2", addr.WebPages[0].StringValue);

            await controller.DoWebsAction(RecordAction.raDelete, addr.WebPages[0]);
            Assert.AreEqual(0, addr.WebPages.Count);
        }

        private static async Task TestMailsActions(AddressEditDlgController controller, GDMAddress addr)
        {
            StdDialogsStub.SetStrInputResult("test mail");
            await controller.DoMailsAction(RecordAction.raAdd, null);
            Assert.AreEqual(1, addr.EmailAddresses.Count);
            Assert.AreEqual("test mail", addr.EmailAddresses[0].StringValue);

            StdDialogsStub.SetStrInputResult("test mail 2");
            await controller.DoMailsAction(RecordAction.raEdit, addr.EmailAddresses[0]);
            Assert.AreEqual(1, addr.EmailAddresses.Count);
            Assert.AreEqual("test mail 2", addr.EmailAddresses[0].StringValue);

            await controller.DoMailsAction(RecordAction.raDelete, addr.EmailAddresses[0]);
            Assert.AreEqual(0, addr.EmailAddresses.Count);
        }

        private static async Task TestPhonesActions(AddressEditDlgController controller, GDMAddress addr)
        {
            StdDialogsStub.SetStrInputResult("test phone");
            await controller.DoPhonesAction(RecordAction.raAdd, null);
            Assert.AreEqual(1, addr.PhoneNumbers.Count);
            Assert.AreEqual("test phone", addr.PhoneNumbers[0].StringValue);

            StdDialogsStub.SetStrInputResult("test phone 2");
            await controller.DoPhonesAction(RecordAction.raEdit, addr.PhoneNumbers[0]);
            Assert.AreEqual(1, addr.PhoneNumbers.Count);
            Assert.AreEqual("test phone 2", addr.PhoneNumbers[0].StringValue);

            await controller.DoPhonesAction(RecordAction.raDelete, addr.PhoneNumbers[0]);
            Assert.AreEqual(0, addr.PhoneNumbers.Count);
        }

        private IAddressEditDlg CreateMockView()
        {
            var view = Substitute.For<IAddressEditDlg>();
            SubstituteControl<IButton>(view, "btnAccept");
            SubstituteControl<IButton>(view, "btnCancel");
            SubstituteControl<ITabPage>(view, "pageCommon");
            SubstituteControl<ILabel>(view, "lblCountry");
            SubstituteControl<ILabel>(view, "lblState");
            SubstituteControl<ILabel>(view, "lblCity");
            SubstituteControl<ILabel>(view, "lblPostalCode");
            SubstituteControl<ILabel>(view, "lblAddress");
            SubstituteControl<ITabPage>(view, "pagePhones");
            SubstituteControl<ITabPage>(view, "pageEmails");
            SubstituteControl<ITabPage>(view, "pageWebPages");

            var sheetSubst = SubstituteSheetList(new TagsListModel(fBaseWin.Context, "phones"));
            view.PhonesList.Returns(sheetSubst);

            sheetSubst = SubstituteSheetList(new TagsListModel(fBaseWin.Context, "mails"));
            view.MailsList.Returns(sheetSubst);

            sheetSubst = SubstituteSheetList(new TagsListModel(fBaseWin.Context, "webs"));
            view.WebsList.Returns(sheetSubst);
            return view;
        }

        private void AssertViewMatchesModel(GDMAddress address, IAddressEditDlg view)
        {
            Assert.AreEqual(address.AddressCountry, view.Country.Text);
            Assert.AreEqual(address.AddressState, view.State.Text);
            Assert.AreEqual(address.AddressCity, view.City.Text);
            Assert.AreEqual(address.AddressPostalCode, view.PostalCode.Text);
            Assert.AreEqual(address.Lines.Text.Trim(), view.AddressLine.Text);
        }
    }
}
