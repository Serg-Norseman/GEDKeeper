using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmOrganizer : Form
	{
        private readonly IBase fBase;
        private readonly GKSheetList fAdrList;
		private readonly GKSheetList fPhonesList;
		private readonly GKSheetList fMailsList;
		private readonly GKSheetList fWebsList;


        private void CollectData()
		{
			this.fAdrList.List.Items.Clear();
			this.fPhonesList.List.Items.Clear();
			this.fMailsList.List.Items.Clear();
			this.fWebsList.List.Items.Clear();

			int num = this.fBase.Tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = this.fBase.Tree[i];

				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord i_rec = rec as TGEDCOMIndividualRecord;
					string nm = i_rec.aux_GetNameStr(true, false);

					int num2 = i_rec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++) {
						this.PrepareEvent(nm, i_rec.IndividualEvents[j]);
					}
				}
			}

			this.fAdrList.List.ResizeColumn(0);
			this.fAdrList.List.ResizeColumn(1);
			this.fPhonesList.List.ResizeColumn(0);
			this.fPhonesList.List.ResizeColumn(1);
			this.fMailsList.List.ResizeColumn(0);
			this.fMailsList.List.ResizeColumn(1);
			this.fWebsList.List.ResizeColumn(0);
			this.fWebsList.List.ResizeColumn(1);
		}

		private void TfmOrganizer_Load(object sender, EventArgs e)
		{
			this.CollectData();
		}

		public TfmOrganizer(IBase aBase)
		{
			this.InitializeComponent();
			
            this.fBase = aBase;
			
            this.fAdrList = new GKSheetList(this.SheetAddresses);
			this.fAdrList.Buttons = EnumSet.Create();
			this.fAdrList.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fAdrList.List.AddListColumn(LangMan.LS(LSID.LSID_Address), 100, false);
			
            this.fPhonesList = new GKSheetList(this.SheetTelephones);
			this.fPhonesList.Buttons = EnumSet.Create();
			this.fPhonesList.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fPhonesList.List.AddListColumn(LangMan.LS(LSID.LSID_Telephone), 100, false);
			
            this.fMailsList = new GKSheetList(this.SheetEMails);
			this.fMailsList.Buttons = EnumSet.Create();
			this.fMailsList.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fMailsList.List.AddListColumn(LangMan.LS(LSID.LSID_Mail), 100, false);
			
            this.fWebsList = new GKSheetList(this.SheetWebs);
			this.fWebsList.Buttons = EnumSet.Create();
			this.fWebsList.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fWebsList.List.AddListColumn(LangMan.LS(LSID.LSID_WebSite), 100, false);
			
			this.Text = LangMan.LS(LSID.LSID_MIOrganizer);
		}

		private static void AddItem(GKListView list, string name, string value)
		{
			GKListItem item = list.AddItem(name, null);
			item.SubItems.Add(value);
		}

		private void PrepareEvent(string iName, TGEDCOMCustomEvent ev)
		{
			TGEDCOMAddress addr = ev.Detail.Address;
			if (addr != null) {
				string addrStr = addr.Address.Text.Trim();
				if (addrStr != "") {
					AddItem(this.fAdrList.List, iName, addrStr);
				}

				int num = addr.PhoneNumbers.Count - 1;
				for (int i = 0; i <= num; i++) {
					AddItem(this.fPhonesList.List, iName, addr.PhoneNumbers[i].StringValue);
				}

				int num2 = addr.EmailAddresses.Count - 1;
				for (int i = 0; i <= num2; i++) {
					AddItem(this.fMailsList.List, iName, addr.EmailAddresses[i].StringValue);
				}

				int num3 = addr.WebPages.Count - 1;
				for (int i = 0; i <= num3; i++) {
					AddItem(this.fWebsList.List, iName, addr.WebPages[i].StringValue);
				}
			}
		}
	}
}
