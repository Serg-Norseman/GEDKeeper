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
	public partial class TfmOrganizer : Form
	{
		private GKSheetList FAdrList;
		private GKSheetList FPhonesList;
		private GKSheetList FMailsList;
		private GKSheetList FWebsList;
		private TfmBase FBase;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private void CollectData()
		{
			this.FAdrList.List.Items.Clear();
			this.FPhonesList.List.Items.Clear();
			this.FMailsList.List.Items.Clear();
			this.FWebsList.List.Items.Clear();

			int num = this.Base.Tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = this.Base.Tree[i];

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
			this.FAdrList.List.ResizeColumn(0);
			this.FAdrList.List.ResizeColumn(1);
			this.FPhonesList.List.ResizeColumn(0);
			this.FPhonesList.List.ResizeColumn(1);
			this.FMailsList.List.ResizeColumn(0);
			this.FMailsList.List.ResizeColumn(1);
			this.FWebsList.List.ResizeColumn(0);
			this.FWebsList.List.ResizeColumn(1);
		}

		private void TfmOrganizer_Load(object sender, EventArgs e)
		{
			this.CollectData();
		}

		public TfmOrganizer(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FAdrList = new GKSheetList(this.SheetAddresses);
			this.FAdrList.Buttons = EnumSet.Create();
			this.FAdrList.List.AddListColumn(LangMan.LSList[96], 350, false);
			this.FAdrList.List.AddListColumn(LangMan.LSList[82], 100, false);
			this.FPhonesList = new GKSheetList(this.SheetTelephones);
			this.FPhonesList.Buttons = EnumSet.Create();
			this.FPhonesList.List.AddListColumn(LangMan.LSList[96], 350, false);
			this.FPhonesList.List.AddListColumn(LangMan.LSList[131], 100, false);
			this.FMailsList = new GKSheetList(this.SheetEMails);
			this.FMailsList.Buttons = EnumSet.Create();
			this.FMailsList.List.AddListColumn(LangMan.LSList[96], 350, false);
			this.FMailsList.List.AddListColumn(LangMan.LSList[132], 100, false);
			this.FWebsList = new GKSheetList(this.SheetWebs);
			this.FWebsList.Buttons = EnumSet.Create();
			this.FWebsList.List.AddListColumn(LangMan.LSList[96], 350, false);
			this.FWebsList.List.AddListColumn(LangMan.LSList[133], 100, false);
			this.Text = LangMan.LSList[34];
		}

		private void AddItem(GKListView aList, string aPerson, string aData)
		{
			GKListItem item = aList.AddItem(aPerson, null);
			item.SubItems.Add(aData);
		}

		private void PrepareEvent(string iName, TGEDCOMCustomEvent ev)
		{
			TGEDCOMAddress addr = ev.Detail.Address;
			if (addr != null) {
				string addr_str = addr.Address.Text.Trim();
				if (addr_str != "") {
					this.AddItem(this.FAdrList.List, iName, addr_str);
				}

				int num = addr.PhoneNumbers.Count - 1;
				for (int i = 0; i <= num; i++) {
					this.AddItem(this.FPhonesList.List, iName, addr.PhoneNumbers[i].StringValue);
				}

				int num2 = addr.EmailAddresses.Count - 1;
				for (int i = 0; i <= num2; i++) {
					this.AddItem(this.FMailsList.List, iName, addr.EmailAddresses[i].StringValue);
				}

				int num3 = addr.WebPages.Count - 1;
				for (int i = 0; i <= num3; i++) {
					this.AddItem(this.FWebsList.List, iName, addr.WebPages[i].StringValue);
				}
			}
		}
	}
}
