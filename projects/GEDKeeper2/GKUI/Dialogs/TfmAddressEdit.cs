/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
    public partial class TfmAddressEdit : Form, IBaseEditor
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

            this.edCountry.Text = this.fAddress.AddressCountry;
            this.edState.Text = this.fAddress.AddressState;
            this.edCity.Text = this.fAddress.AddressCity;
            this.edPostalCode.Text = this.fAddress.AddressPostalCode;
            this.edAddress.Text = this.fAddress.Address.Text.Trim();

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
                this.fAddress.AddressCountry = this.edCountry.Text;
                this.fAddress.AddressState = this.edState.Text;
                this.fAddress.AddressCity = this.edCity.Text;
                this.fAddress.AddressPostalCode = this.edPostalCode.Text;

                this.fAddress.SetAddressText(this.edAddress.Text);

                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmAddressEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public TfmAddressEdit(IBaseWindow aBase)
        {
            this.InitializeComponent();
            this.fBase = aBase;

            this.fPhonesList = new GKSheetList(this.SheetPhones);
            this.fPhonesList.OnModify += this.ListModify;
            this.fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Telephone), 350, false);

            this.fMailsList = new GKSheetList(this.SheetEmails);
            this.fMailsList.OnModify += this.ListModify;
            this.fMailsList.AddColumn(LangMan.LS(LSID.LSID_Mail), 350, false);

            this.fWebsList = new GKSheetList(this.SheetWebPages);
            this.fWebsList.OnModify += this.ListModify;
            this.fWebsList.AddColumn(LangMan.LS(LSID.LSID_WebSite), 350, false);

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_Address);
            this.SheetCommon.Text = LangMan.LS(LSID.LSID_Address);
            this.Label1.Text = LangMan.LS(LSID.LSID_AdCountry);
            this.Label2.Text = LangMan.LS(LSID.LSID_AdState);
            this.Label3.Text = LangMan.LS(LSID.LSID_AdCity);
            this.Label4.Text = LangMan.LS(LSID.LSID_AdPostalCode);
            this.Label5.Text = LangMan.LS(LSID.LSID_Address);
            this.SheetPhones.Text = LangMan.LS(LSID.LSID_Telephones);
            this.SheetEmails.Text = LangMan.LS(LSID.LSID_EMails);
            this.SheetWebPages.Text = LangMan.LS(LSID.LSID_WebSites);
        }
    }
}
