using System;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
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
				GEDCOMRecord rec = this.fBase.Tree[i];
			    if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;
			    
                GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
			    string nm = iRec.aux_GetNameStr(true, false);

			    int num2 = iRec.IndividualEvents.Count - 1;
			    for (int j = 0; j <= num2; j++) {
			        this.PrepareEvent(nm, iRec.IndividualEvents[j]);
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
            this.fAdrList.Buttons.Clear();
			this.fAdrList.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fAdrList.List.AddListColumn(LangMan.LS(LSID.LSID_Address), 100, false);
			
            this.fPhonesList = new GKSheetList(this.SheetTelephones);
			this.fPhonesList.Buttons.Clear();
			this.fPhonesList.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fPhonesList.List.AddListColumn(LangMan.LS(LSID.LSID_Telephone), 100, false);
			
            this.fMailsList = new GKSheetList(this.SheetEMails);
			this.fMailsList.Buttons.Clear();
			this.fMailsList.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fMailsList.List.AddListColumn(LangMan.LS(LSID.LSID_Mail), 100, false);
			
            this.fWebsList = new GKSheetList(this.SheetWebs);
			this.fWebsList.Buttons.Clear();
			this.fWebsList.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fWebsList.List.AddListColumn(LangMan.LS(LSID.LSID_WebSite), 100, false);
			
			this.Text = LangMan.LS(LSID.LSID_MIOrganizer);
		}

		private static void AddItem(GKListView list, string name, string value)
		{
			GKListItem item = list.AddItem(name, null);
			item.SubItems.Add(value);
		}

		private void PrepareEvent(string iName, GEDCOMCustomEvent ev)
		{
			GEDCOMAddress addr = ev.Detail.Address;
		    if (addr == null) return;
		    
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
