/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Lists;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class AddressEditDlg : EditorDialog, IAddressEditDlg
    {
        private GEDCOMAddress fAddress;
        private readonly GKSheetList fPhonesList;
        private readonly GKSheetList fMailsList;
        private readonly GKSheetList fWebsList;

        public GEDCOMAddress Address
        {
            get { return fAddress; }
            set { SetAddress(value); }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMTag itemTag = eArgs.ItemData as GEDCOMTag;
            if ((eArgs.Action == RecordAction.raEdit || eArgs.Action == RecordAction.raDelete) && (itemTag == null)) return;

            string val;
            if (sender == fPhonesList)
            {
                switch (eArgs.Action) {
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
            }
            else if (sender == fMailsList)
            {
                switch (eArgs.Action) {
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
            }
            else if (sender == fWebsList)
            {
                switch (eArgs.Action) {
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
            }

            UpdateLists();
        }

        private void SetAddress(GEDCOMAddress value)
        {
            fAddress = value;

            txtCountry.Text = fAddress.AddressCountry;
            txtState.Text = fAddress.AddressState;
            txtCity.Text = fAddress.AddressCity;
            txtPostalCode.Text = fAddress.AddressPostalCode;
            txtAddress.Text = fAddress.Address.Text.Trim();

            UpdateLists();
        }

        private void UpdateLists()
        {
            fPhonesList.ClearItems();
            foreach (GEDCOMTag tag in fAddress.PhoneNumbers)
            {
                fPhonesList.AddItem(tag, tag.StringValue);
            }

            fMailsList.ClearItems();
            foreach (GEDCOMTag tag in fAddress.EmailAddresses)
            {
                fMailsList.AddItem(tag, tag.StringValue);
            }

            fWebsList.ClearItems();
            foreach (GEDCOMTag tag in fAddress.WebPages)
            {
                fWebsList.AddItem(tag, tag.StringValue);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                fAddress.AddressCountry = txtCountry.Text;
                fAddress.AddressState = txtState.Text;
                fAddress.AddressCity = txtCity.Text;
                fAddress.AddressPostalCode = txtPostalCode.Text;

                fAddress.SetAddressText(txtAddress.Text);

                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("AddressEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        public AddressEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            fPhonesList = new GKSheetList(pagePhones);
            fPhonesList.SetControlName("fPhonesList"); // for purpose of tests
            fPhonesList.OnModify += ListModify;
            fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Telephone), 350, false);

            fMailsList = new GKSheetList(pageEmails);
            fMailsList.SetControlName("fMailsList"); // for purpose of tests
            fMailsList.OnModify += ListModify;
            fMailsList.AddColumn(LangMan.LS(LSID.LSID_Mail), 350, false);

            fWebsList = new GKSheetList(pageWebPages);
            fWebsList.SetControlName("fWebsList"); // for purpose of tests
            fWebsList.OnModify += ListModify;
            fWebsList.AddColumn(LangMan.LS(LSID.LSID_WebSite), 350, false);

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_Address);
            pageCommon.Text = LangMan.LS(LSID.LSID_Address);
            lblCountry.Text = LangMan.LS(LSID.LSID_AdCountry);
            lblState.Text = LangMan.LS(LSID.LSID_AdState);
            lblCity.Text = LangMan.LS(LSID.LSID_AdCity);
            lblPostalCode.Text = LangMan.LS(LSID.LSID_AdPostalCode);
            lblAddress.Text = LangMan.LS(LSID.LSID_Address);
            pagePhones.Text = LangMan.LS(LSID.LSID_Telephones);
            pageEmails.Text = LangMan.LS(LSID.LSID_EMails);
            pageWebPages.Text = LangMan.LS(LSID.LSID_WebSites);
        }
    }
}
