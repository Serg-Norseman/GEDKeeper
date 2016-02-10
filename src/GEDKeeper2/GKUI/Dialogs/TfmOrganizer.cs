using System;
using System.Windows.Forms;

using BSLib;
using GKCommon.GEDCOM;
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
        private readonly IBaseWindow fBase;
        private readonly GKSheetList fAdrList;
		private readonly GKSheetList fPhonesList;
		private readonly GKSheetList fMailsList;
		private readonly GKSheetList fWebsList;

		public TfmOrganizer(IBaseWindow aBase)
		{
			this.InitializeComponent();
			
            this.fBase = aBase;
			
            this.fAdrList = new GKSheetList(this.SheetAddresses);
            this.fAdrList.Buttons = EnumSet<SheetButton>.Create();
			this.fAdrList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fAdrList.AddColumn(LangMan.LS(LSID.LSID_Address), 100, false);
			
            this.fPhonesList = new GKSheetList(this.SheetTelephones);
			this.fPhonesList.Buttons = EnumSet<SheetButton>.Create();
			this.fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Telephone), 100, false);
			
            this.fMailsList = new GKSheetList(this.SheetEMails);
			this.fMailsList.Buttons = EnumSet<SheetButton>.Create();
			this.fMailsList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fMailsList.AddColumn(LangMan.LS(LSID.LSID_Mail), 100, false);
			
            this.fWebsList = new GKSheetList(this.SheetWebs);
			this.fWebsList.Buttons = EnumSet<SheetButton>.Create();
			this.fWebsList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
			this.fWebsList.AddColumn(LangMan.LS(LSID.LSID_WebSite), 100, false);
			
			this.Text = LangMan.LS(LSID.LSID_MIOrganizer);
		}

		private void TfmOrganizer_Load(object sender, EventArgs e)
		{
			this.CollectData();
		}

        private void CollectData()
		{
			this.fAdrList.ClearItems();
			this.fPhonesList.ClearItems();
			this.fMailsList.ClearItems();
			this.fWebsList.ClearItems();

			int num = this.fBase.Tree.RecordsCount;
			for (int i = 0; i < num; i++)
			{
				GEDCOMRecord rec = this.fBase.Tree[i];
			    if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;

                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;
			    string nm = iRec.GetNameString(true, false);

			    int num2 = iRec.Events.Count;
			    for (int j = 0; j < num2; j++) {
			        this.PrepareEvent(nm, iRec.Events[j]);
			    }
			}

			this.fAdrList.ResizeColumn(0);
			this.fAdrList.ResizeColumn(1);
			this.fPhonesList.ResizeColumn(0);
			this.fPhonesList.ResizeColumn(1);
			this.fMailsList.ResizeColumn(0);
			this.fMailsList.ResizeColumn(1);
			this.fWebsList.ResizeColumn(0);
			this.fWebsList.ResizeColumn(1);
		}

		private static void AddItem(GKSheetList list, string name, string value)
		{
			GKListItem item = list.AddItem(name, null);
			item.AddSubItem(value);
		}

		private void PrepareEvent(string iName, GEDCOMCustomEvent ev)
		{
			GEDCOMAddress addr = ev.Detail.Address;
		    if (addr == null) return;
		    
            string addrStr = addr.Address.Text.Trim();
		    if (addrStr != "") {
		        AddItem(this.fAdrList, iName, addrStr);
		    }

		    foreach (GEDCOMTag tag in addr.PhoneNumbers) {
		        AddItem(this.fPhonesList, iName, tag.StringValue);
		    }

		    foreach (GEDCOMTag tag in addr.EmailAddresses) {
		        AddItem(this.fMailsList, iName, tag.StringValue);
		    }

		    foreach (GEDCOMTag tag in addr.WebPages) {
		        AddItem(this.fWebsList, iName, tag.StringValue);
		    }
		}
	}
}
