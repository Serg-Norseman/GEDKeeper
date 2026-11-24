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
            var baseWin = GetBaseSubst();

            IAddressEditDlg view = CreateMockView();
            var controller = new AddressEditDlgController(view);
            controller.InitView(baseWin);
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
            AssertViewMatchesModel(addr, controller);

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

        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageCommon;
        private ITabPage _pagePhones;
        private ITabPage _pageEmails;
        private ITabPage _pageWebPages;
        private ILabel _lblCountry;
        private ILabel _lblState;
        private ILabel _lblCity;
        private ILabel _lblPostalCode;
        private ILabel _lblAddress;
        private ITextBox _txtCountry;
        private ITextBox _txtState;
        private ITextBox _txtCity;
        private ITextBox _txtPostalCode;
        private ITextBox _txtAddress;
        private ISheetList _fPhonesList;
        private ISheetList _fMailsList;
        private ISheetList _fWebsList;

        private IAddressEditDlg CreateMockView()
        {
            var view = Substitute.For<IAddressEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _pageCommon = SubstCtl<ITabPage>(view, "pageCommon");
            _lblCountry = SubstCtl<ILabel>(view, "lblCountry");
            _lblState = SubstCtl<ILabel>(view, "lblState");
            _lblCity = SubstCtl<ILabel>(view, "lblCity");
            _lblPostalCode = SubstCtl<ILabel>(view, "lblPostalCode");
            _lblAddress = SubstCtl<ILabel>(view, "lblAddress");
            _pagePhones = SubstCtl<ITabPage>(view, "pagePhones");
            _pageEmails = SubstCtl<ITabPage>(view, "pageEmails");
            _pageWebPages = SubstCtl<ITabPage>(view, "pageWebPages");
            _txtCountry = SubstCtl<ITextBox>(view, "txtCountry");
            _txtState = SubstCtl<ITextBox>(view, "txtState");
            _txtCity = SubstCtl<ITextBox>(view, "txtCity");
            _txtPostalCode = SubstCtl<ITextBox>(view, "txtPostalCode");
            _txtAddress = SubstCtl<ITextBox>(view, "txtAddress");

            _fPhonesList = SubstituteSheetList(new TagsListModel(fBaseWin.Context, "phones"));
            SubstituteControl(view, "fPhonesList", _fPhonesList);

            _fMailsList = SubstituteSheetList(new TagsListModel(fBaseWin.Context, "mails"));
            SubstituteControl(view, "fMailsList", _fMailsList);

            _fWebsList = SubstituteSheetList(new TagsListModel(fBaseWin.Context, "webs"));
            SubstituteControl(view, "fWebsList", _fWebsList);

            return view;
        }

        private void AssertViewMatchesModel(GDMAddress address, AddressEditDlgController controller)
        {
            Assert.AreEqual(address.AddressCountry, _txtCountry.Text);
            Assert.AreEqual(address.AddressState, _txtState.Text);
            Assert.AreEqual(address.AddressCity, _txtCity.Text);
            Assert.AreEqual(address.AddressPostalCode, _txtPostalCode.Text);
            Assert.AreEqual(address.Lines.Text.Trim(), _txtAddress.Text);
        }
    }
}
