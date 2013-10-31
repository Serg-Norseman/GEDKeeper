using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmAddressEdit : Form
	{
		private TGEDCOMAddress FAddress;
		private GKSheetList FPhonesList;
		private GKSheetList FMailsList;
		private GKSheetList FWebsList;

		public TGEDCOMAddress Address
		{
			get { return this.FAddress; }
			set { this.SetAddress(value); }
		}

		private bool GetInput(string aTitle, ref string aValue)
		{
			return GKInputBox.QueryText(aTitle, LangMan.LSList[202], ref aValue) && aValue.Trim() != "";
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
            TGEDCOMTag itemTag = eArgs.ItemData as TGEDCOMTag;
            if ((eArgs.Action == TRecAction.raEdit || eArgs.Action == TRecAction.raDelete) && (itemTag == null)) return;

            string val;
            if (sender == this.FPhonesList)
            {
            	switch (eArgs.Action) {
            		case TRecAction.raAdd:
            			val = "";
            			if (GetInput(LangMan.LSList[131], ref val)) {
            				this.FAddress.AddPhoneNumber(val);
            			}
            			break;

            		case TRecAction.raEdit:
            			val = itemTag.StringValue;
            			if (GetInput(LangMan.LSList[131], ref val)) {
            				itemTag.StringValue = val;
            			}
            			break;

            		case TRecAction.raDelete:
            			this.FAddress.PhoneNumbers.DeleteObject(itemTag);
            			break;
            	}
            }
            else if (sender == this.FMailsList)
            {
            	switch (eArgs.Action) {
            		case TRecAction.raAdd:
            			val = "";
            			if (GetInput(LangMan.LSList[132], ref val)) {
            				this.FAddress.AddEmailAddress(val);
            			}
            			break;

            		case TRecAction.raEdit:
            			val = itemTag.StringValue;
            			if (GetInput(LangMan.LSList[132], ref val)) {
            				itemTag.StringValue = val;
            			}
            			break;

            		case TRecAction.raDelete:
            			this.FAddress.EmailAddresses.DeleteObject(itemTag);
            			break;
            	}
            }
            else if (sender == this.FWebsList)
            {
            	switch (eArgs.Action) {
            		case TRecAction.raAdd:
            			val = "";
            			if (GetInput(LangMan.LSList[133], ref val)) {
            				this.FAddress.AddWebPage(val);
            			}
            			break;

            		case TRecAction.raEdit:
            			val = itemTag.StringValue;
            			if (GetInput(LangMan.LSList[133], ref val)) {
            				itemTag.StringValue = val;
            			}
            			break;

            		case TRecAction.raDelete:
            			this.FAddress.WebPages.DeleteObject(itemTag);
            			break;
            	}
            }

			this.UpdateLists();
		}

		private void SetAddress(TGEDCOMAddress Value)
		{
			this.FAddress = Value;
			this.edCountry.Text = this.FAddress.AddressCountry;
			this.edState.Text = this.FAddress.AddressState;
			this.edCity.Text = this.FAddress.AddressCity;
			this.edPostalCode.Text = this.FAddress.AddressPostalCode;
			this.edAddress.Text = this.FAddress.Address.Text.Trim();
			this.UpdateLists();
		}

		private void UpdateLists()
		{
			this.FPhonesList.List.Items.Clear();
			this.FMailsList.List.Items.Clear();
			this.FWebsList.List.Items.Clear();

			TGEDCOMTag tag;

			int num = this.FAddress.PhoneNumbers.Count - 1;
			for (int i = 0; i <= num; i++) {
				tag = this.FAddress.PhoneNumbers[i];
				this.FPhonesList.List.AddItem(tag.StringValue, tag);
			}

			int num2 = this.FAddress.EmailAddresses.Count - 1;
			for (int i = 0; i <= num2; i++) {
				tag = this.FAddress.EmailAddresses[i];
				this.FMailsList.List.AddItem(tag.StringValue, tag);
			}

			int num3 = this.FAddress.WebPages.Count - 1;
			for (int i = 0; i <= num3; i++) {
				tag = this.FAddress.WebPages[i];
				this.FWebsList.List.AddItem(tag.StringValue, tag);
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.FAddress.AddressCountry = this.edCountry.Text;
				this.FAddress.AddressState = this.edState.Text;
				this.FAddress.AddressCity = this.edCity.Text;
				this.FAddress.AddressPostalCode = this.edPostalCode.Text;

				this.FAddress.aux_SetAddressValue(this.edAddress.Text);

				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmAddressEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmAddressEdit()
		{
			this.InitializeComponent();

			this.FPhonesList = new GKSheetList(this.SheetPhones);
			this.FPhonesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.FPhonesList.List.AddListColumn(LangMan.LSList[131], 350, false);

			this.FMailsList = new GKSheetList(this.SheetEmails);
			this.FMailsList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.FMailsList.List.AddListColumn(LangMan.LSList[132], 350, false);

			this.FWebsList = new GKSheetList(this.SheetWebPages);
			this.FWebsList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.FWebsList.List.AddListColumn(LangMan.LSList[133], 350, false);

			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[82];
			this.SheetCommon.Text = LangMan.LSList[82];
			this.Label1.Text = LangMan.LSList[195];
			this.Label2.Text = LangMan.LSList[196];
			this.Label3.Text = LangMan.LSList[197];
			this.Label4.Text = LangMan.LSList[198];
			this.Label5.Text = LangMan.LSList[82];
			this.SheetPhones.Text = LangMan.LSList[199];
			this.SheetEmails.Text = LangMan.LSList[200];
			this.SheetWebPages.Text = LangMan.LSList[201];
		}
	}
}
