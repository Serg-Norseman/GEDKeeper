/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
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
                Logger.LogWrite("AddressEditController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Country.Text = fAddress.AddressCountry;
            fView.State.Text = fAddress.AddressState;
            fView.City.Text = fAddress.AddressCity;
            fView.PostalCode.Text = fAddress.AddressPostalCode;
            fView.AddressLine.Text = fAddress.Address.Text.Trim();

            UpdateLists();
        }

        public void UpdateLists()
        {
            fView.PhonesList.ClearItems();
            foreach (GDMTag tag in fAddress.PhoneNumbers) {
                fView.PhonesList.AddItem(tag, tag.StringValue);
            }

            fView.MailsList.ClearItems();
            foreach (GDMTag tag in fAddress.EmailAddresses) {
                fView.MailsList.AddItem(tag, tag.StringValue);
            }

            fView.WebsList.ClearItems();
            foreach (GDMTag tag in fAddress.WebPages) {
                fView.WebsList.AddItem(tag, tag.StringValue);
            }
        }

        public void DoPhonesAction(RecordAction action, GDMTag itemTag)
        {
            string val;
            switch (action) {
                case RecordAction.raAdd:
                    val = "";
                    if (AppHost.StdDialogs.GetInput(LangMan.LS(LSID.LSID_Telephone), ref val)) {
                        fAddress.AddPhoneNumber(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (AppHost.StdDialogs.GetInput(LangMan.LS(LSID.LSID_Telephone), ref val)) {
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
                    if (AppHost.StdDialogs.GetInput(LangMan.LS(LSID.LSID_Mail), ref val)) {
                        fAddress.AddEmailAddress(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (AppHost.StdDialogs.GetInput(LangMan.LS(LSID.LSID_Mail), ref val)) {
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
                    if (AppHost.StdDialogs.GetInput(LangMan.LS(LSID.LSID_WebSite), ref val)) {
                        fAddress.AddWebPage(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (AppHost.StdDialogs.GetInput(LangMan.LS(LSID.LSID_WebSite), ref val)) {
                        itemTag.StringValue = val;
                    }
                    break;

                case RecordAction.raDelete:
                    fAddress.WebPages.Delete(itemTag);
                    break;
            }
            UpdateLists();
        }
    }
}
