/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class AddressEditDlgController : DialogController<IAddressEditDlg>
    {
        private GDMAddress fAddress;

        public GDMAddress Address
        {
            get { return fAddress; }
            set {
                if (fAddress != value) {
                    fAddress = value;
                    UpdateView();
                }
            }
        }


        public AddressEditDlgController(IAddressEditDlg view) : base(view)
        {
        }

        public override bool Accept()
        {
            try {
                fAddress.AddressCountry = fView.Country.Text;
                fAddress.AddressState = fView.State.Text;
                fAddress.AddressCity = fView.City.Text;
                fAddress.AddressPostalCode = fView.PostalCode.Text;
                fAddress.SetAddressText(fView.AddressLine.Text);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("AddressEditController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Country.Text = fAddress.AddressCountry;
            fView.State.Text = fAddress.AddressState;
            fView.City.Text = fAddress.AddressCity;
            fView.PostalCode.Text = fAddress.AddressPostalCode;
            fView.AddressLine.Text = fAddress.Lines.Text.Trim();

            UpdateLists();
        }

        public void UpdateLists()
        {
            fView.PhonesList.ListView.ClearItems();
            foreach (GDMTag tag in fAddress.PhoneNumbers) {
                fView.PhonesList.ListView.AddItem(tag, tag.StringValue);
            }

            fView.MailsList.ListView.ClearItems();
            foreach (GDMTag tag in fAddress.EmailAddresses) {
                fView.MailsList.ListView.AddItem(tag, tag.StringValue);
            }

            fView.WebsList.ListView.ClearItems();
            foreach (GDMTag tag in fAddress.WebPages) {
                fView.WebsList.ListView.AddItem(tag, tag.StringValue);
            }
        }

        public void DoPhonesAction(RecordAction action, GDMTag itemTag)
        {
            string val;
            switch (action) {
                case RecordAction.raAdd:
                    val = "";
                    if (AppHost.StdDialogs.GetInput(fView, LangMan.LS(LSID.LSID_Telephone), ref val)) {
                        fAddress.AddPhoneNumber(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (AppHost.StdDialogs.GetInput(fView, LangMan.LS(LSID.LSID_Telephone), ref val)) {
                        itemTag.StringValue = val;
                    }
                    break;

                case RecordAction.raDelete:
                    fAddress.PhoneNumbers.Delete(itemTag);
                    break;
            }
            UpdateLists();
        }

        public void DoMailsAction(RecordAction action, GDMTag itemTag)
        {
            string val;
            switch (action) {
                case RecordAction.raAdd:
                    val = "";
                    if (AppHost.StdDialogs.GetInput(fView, LangMan.LS(LSID.LSID_Mail), ref val)) {
                        fAddress.AddEmailAddress(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (AppHost.StdDialogs.GetInput(fView, LangMan.LS(LSID.LSID_Mail), ref val)) {
                        itemTag.StringValue = val;
                    }
                    break;

                case RecordAction.raDelete:
                    fAddress.EmailAddresses.Delete(itemTag);
                    break;
            }
            UpdateLists();
        }

        public void DoWebsAction(RecordAction action, GDMTag itemTag)
        {
            string val;
            switch (action) {
                case RecordAction.raAdd:
                    val = "";
                    if (AppHost.StdDialogs.GetInput(fView, LangMan.LS(LSID.LSID_WebSite), ref val)) {
                        fAddress.AddWebPage(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (AppHost.StdDialogs.GetInput(fView, LangMan.LS(LSID.LSID_WebSite), ref val)) {
                        itemTag.StringValue = val;
                    }
                    break;

                case RecordAction.raDelete:
                    fAddress.WebPages.Delete(itemTag);
                    break;
            }
            UpdateLists();
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_Address);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.LSID_Address);
            GetControl<ILabel>("lblCountry").Text = LangMan.LS(LSID.LSID_AdCountry);
            GetControl<ILabel>("lblState").Text = LangMan.LS(LSID.LSID_AdState);
            GetControl<ILabel>("lblCity").Text = LangMan.LS(LSID.LSID_AdCity);
            GetControl<ILabel>("lblPostalCode").Text = LangMan.LS(LSID.LSID_AdPostalCode);
            GetControl<ILabel>("lblAddress").Text = LangMan.LS(LSID.LSID_Address);
            GetControl<ITabPage>("pagePhones").Text = LangMan.LS(LSID.LSID_Telephones);
            GetControl<ITabPage>("pageEmails").Text = LangMan.LS(LSID.LSID_EMails);
            GetControl<ITabPage>("pageWebPages").Text = LangMan.LS(LSID.LSID_WebSites);

            fView.PhonesList.ListView.AddColumn(LangMan.LS(LSID.LSID_Telephone), 350, false);
            fView.MailsList.ListView.AddColumn(LangMan.LS(LSID.LSID_Mail), 350, false);
            fView.WebsList.ListView.AddColumn(LangMan.LS(LSID.LSID_WebSite), 350, false);
        }
    }
}
