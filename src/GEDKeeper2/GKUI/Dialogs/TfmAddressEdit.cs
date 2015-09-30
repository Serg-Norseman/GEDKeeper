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
        private readonly IBase fBase;
		private GEDCOMAddress fAddress;
		private readonly GKSheetList fPhonesList;
		private readonly GKSheetList fMailsList;
		private readonly GKSheetList fWebsList;

		public IBase Base
		{
			get { return this.fBase; }
		}

		public GEDCOMAddress Address
		{
			get { return this.fAddress; }
			set { this.SetAddress(value); }
		}

		private static bool GetInput(string title, ref string value)
		{
            bool res = GKInputBox.QueryText(title, LangMan.LS(LSID.LSID_Value), ref value);
            return res && !string.IsNullOrEmpty(value);
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
            			if (GetInput(LangMan.LS(LSID.LSID_Telephone), ref val)) {
            				this.fAddress.AddPhoneNumber(val);
            			}
            			break;

            		case RecordAction.raEdit:
            			val = itemTag.StringValue;
            			if (GetInput(LangMan.LS(LSID.LSID_Telephone), ref val)) {
            				itemTag.StringValue = val;
            			}
            			break;

            		case RecordAction.raDelete:
            			this.fAddress.PhoneNumbers.DeleteObject(itemTag);
            			break;
            	}
            }
            else if (sender == this.fMailsList)
            {
            	switch (eArgs.Action) {
            		case RecordAction.raAdd:
            			val = "";
            			if (GetInput(LangMan.LS(LSID.LSID_Mail), ref val)) {
            				this.fAddress.AddEmailAddress(val);
            			}
            			break;

            		case RecordAction.raEdit:
            			val = itemTag.StringValue;
            			if (GetInput(LangMan.LS(LSID.LSID_Mail), ref val)) {
            				itemTag.StringValue = val;
            			}
            			break;

            		case RecordAction.raDelete:
            			this.fAddress.EmailAddresses.DeleteObject(itemTag);
            			break;
            	}
            }
            else if (sender == this.fWebsList)
            {
            	switch (eArgs.Action) {
            		case RecordAction.raAdd:
            			val = "";
            			if (GetInput(LangMan.LS(LSID.LSID_WebSite), ref val)) {
            				this.fAddress.AddWebPage(val);
            			}
            			break;

            		case RecordAction.raEdit:
            			val = itemTag.StringValue;
            			if (GetInput(LangMan.LS(LSID.LSID_WebSite), ref val)) {
            				itemTag.StringValue = val;
            			}
            			break;

            		case RecordAction.raDelete:
            			this.fAddress.WebPages.DeleteObject(itemTag);
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
			this.fPhonesList.List.Items.Clear();
			this.fMailsList.List.Items.Clear();
			this.fWebsList.List.Items.Clear();

			GEDCOMTag tag;

			int num = this.fAddress.PhoneNumbers.Count;
			for (int i = 0; i < num; i++) {
				tag = this.fAddress.PhoneNumbers[i];
				this.fPhonesList.List.AddItem(tag.StringValue, tag);
			}

			int num2 = this.fAddress.EmailAddresses.Count;
			for (int i = 0; i < num2; i++) {
				tag = this.fAddress.EmailAddresses[i];
				this.fMailsList.List.AddItem(tag.StringValue, tag);
			}

			int num3 = this.fAddress.WebPages.Count;
			for (int i = 0; i < num3; i++) {
				tag = this.fAddress.WebPages[i];
				this.fWebsList.List.AddItem(tag.StringValue, tag);
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
				this.fBase.Host.LogWrite("TfmAddressEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmAddressEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			this.fPhonesList = new GKSheetList(this.SheetPhones);
			this.fPhonesList.OnModify += this.ListModify;
			this.fPhonesList.List.AddListColumn(LangMan.LS(LSID.LSID_Telephone), 350, false);

			this.fMailsList = new GKSheetList(this.SheetEmails);
			this.fMailsList.OnModify += this.ListModify;
			this.fMailsList.List.AddListColumn(LangMan.LS(LSID.LSID_Mail), 350, false);

			this.fWebsList = new GKSheetList(this.SheetWebPages);
			this.fWebsList.OnModify += this.ListModify;
			this.fWebsList.List.AddListColumn(LangMan.LS(LSID.LSID_WebSite), 350, false);

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
