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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class AddressEditDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private GEDCOMAddress fAddress;
        private readonly GKSheetList fPhonesList;
        private readonly GKSheetList fMailsList;
        private readonly GKSheetList fWebsList;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public GEDCOMAddress Address
        {
            get { return this.fAddress; }
            set { this.SetAddress(value); }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMTag itemTag = eArgs.ItemData as GEDCOMTag;
            if ((eArgs.Action == RecordAction.raEdit || eArgs.Action == RecordAction.raDelete) && (itemTag == null)) return;

            string val;
            if (sender == this.fPhonesList)
            {
                switch (eArgs.Action) {
                    case RecordAction.raAdd:
                        val = "";
                        if (GKUtils.GetInput(LangMan.LS(LSID.LSID_Telephone), ref val)) {
                            this.fAddress.AddPhoneNumber(val);
                        }
                        break;

                    case RecordAction.raEdit:
                        val = itemTag.StringValue;
                        if (GKUtils.GetInput(LangMan.LS(LSID.LSID_Telephone), ref val)) {
                            itemTag.StringValue = val;
                        }
                        break;

                    case RecordAction.raDelete:
                        this.fAddress.PhoneNumbers.Delete(itemTag);
                        break;
                }
            }
            else if (sender == this.fMailsList)
            {
                switch (eArgs.Action) {
                    case RecordAction.raAdd:
                        val = "";
                        if (GKUtils.GetInput(LangMan.LS(LSID.LSID_Mail), ref val)) {
                            this.fAddress.AddEmailAddress(val);
                        }
                        break;

                    case RecordAction.raEdit:
                        val = itemTag.StringValue;
                        if (GKUtils.GetInput(LangMan.LS(LSID.LSID_Mail), ref val)) {
                            itemTag.StringValue = val;
                        }
                        break;

                    case RecordAction.raDelete:
                        this.fAddress.EmailAddresses.Delete(itemTag);
                        break;
                }
            }
            else if (sender == this.fWebsList)
            {
                switch (eArgs.Action) {
                    case RecordAction.raAdd:
                        val = "";
                        if (GKUtils.GetInput(LangMan.LS(LSID.LSID_WebSite), ref val)) {
                            this.fAddress.AddWebPage(val);
                        }
                        break;

                    case RecordAction.raEdit:
                        val = itemTag.StringValue;
                        if (GKUtils.GetInput(LangMan.LS(LSID.LSID_WebSite), ref val)) {
                            itemTag.StringValue = val;
                        }
                        break;

                    case RecordAction.raDelete:
                        this.fAddress.WebPages.Delete(itemTag);
                        break;
                }
            }

            this.UpdateLists();
        }

        private void SetAddress(GEDCOMAddress value)
        {
            this.fAddress = value;

            this.txtCountry.Text = this.fAddress.AddressCountry;
            this.txtState.Text = this.fAddress.AddressState;
            this.txtCity.Text = this.fAddress.AddressCity;
            this.txtPostalCode.Text = this.fAddress.AddressPostalCode;
            this.txtAddress.Text = this.fAddress.Address.Text.Trim();

            this.UpdateLists();
        }

        private void UpdateLists()
        {
            this.fPhonesList.ClearItems();
            foreach (GEDCOMTag tag in this.fAddress.PhoneNumbers)
            {
                this.fPhonesList.AddItem(tag.StringValue, tag);
            }

            this.fMailsList.ClearItems();
            foreach (GEDCOMTag tag in this.fAddress.EmailAddresses)
            {
                this.fMailsList.AddItem(tag.StringValue, tag);
            }

            this.fWebsList.ClearItems();
            foreach (GEDCOMTag tag in this.fAddress.WebPages)
            {
                this.fWebsList.AddItem(tag.StringValue, tag);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fAddress.AddressCountry = this.txtCountry.Text;
                this.fAddress.AddressState = this.txtState.Text;
                this.fAddress.AddressCity = this.txtCity.Text;
                this.fAddress.AddressPostalCode = this.txtPostalCode.Text;

                this.fAddress.SetAddressText(this.txtAddress.Text);

                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("AddressEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public AddressEditDlg(IBaseWindow baseWin)
        {
            this.InitializeComponent();

            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;

            this.fBase = baseWin;

            this.fPhonesList = new GKSheetList(this.pagePhones);
            this.fPhonesList.SetControlName("fPhonesList"); // for purpose of tests
            this.fPhonesList.OnModify += this.ListModify;
            this.fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Telephone), 350, false);

            this.fMailsList = new GKSheetList(this.pageEmails);
            this.fMailsList.SetControlName("fMailsList"); // for purpose of tests
            this.fMailsList.OnModify += this.ListModify;
            this.fMailsList.AddColumn(LangMan.LS(LSID.LSID_Mail), 350, false);

            this.fWebsList = new GKSheetList(this.pageWebPages);
            this.fWebsList.SetControlName("fWebsList"); // for purpose of tests
            this.fWebsList.OnModify += this.ListModify;
            this.fWebsList.AddColumn(LangMan.LS(LSID.LSID_WebSite), 350, false);

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_Address);
            this.pageCommon.Text = LangMan.LS(LSID.LSID_Address);
            this.lblCountry.Text = LangMan.LS(LSID.LSID_AdCountry);
            this.lblState.Text = LangMan.LS(LSID.LSID_AdState);
            this.lblCity.Text = LangMan.LS(LSID.LSID_AdCity);
            this.lblPostalCode.Text = LangMan.LS(LSID.LSID_AdPostalCode);
            this.lblAddress.Text = LangMan.LS(LSID.LSID_Address);
            this.pagePhones.Text = LangMan.LS(LSID.LSID_Telephones);
            this.pageEmails.Text = LangMan.LS(LSID.LSID_EMails);
            this.pageWebPages.Text = LangMan.LS(LSID.LSID_WebSites);
        }
    }
}
