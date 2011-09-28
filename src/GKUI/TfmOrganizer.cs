using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;
using GKUI.Lists;

namespace GKUI
{
	public partial class TfmOrganizer : Form
	{
		private TSheetList FAdrList;
		private TSheetList FPhonesList;
		private TSheetList FMailsList;
		private TSheetList FWebsList;
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
				TGEDCOMRecord rec = this.Base.Tree.GetRecord(i);
				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
					string nm = TGenEngine.GetNameStr(i_rec, true, false);

					int num2 = i_rec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						_CollectData_PrepareEvent(this, nm, i_rec.IndividualEvents[j]);
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
			this.FAdrList = new TSheetList(this.SheetAddresses);
			this.FAdrList.Buttons = TEnumSet.Create();
			this.FAdrList.List.AddListColumn(GKL.LSList[96], 350, false);
			this.FAdrList.List.AddListColumn(GKL.LSList[82], 100, false);
			this.FPhonesList = new TSheetList(this.SheetTelephones);
			this.FPhonesList.Buttons = TEnumSet.Create();
			this.FPhonesList.List.AddListColumn(GKL.LSList[96], 350, false);
			this.FPhonesList.List.AddListColumn(GKL.LSList[131], 100, false);
			this.FMailsList = new TSheetList(this.SheetEMails);
			this.FMailsList.Buttons = TEnumSet.Create();
			this.FMailsList.List.AddListColumn(GKL.LSList[96], 350, false);
			this.FMailsList.List.AddListColumn(GKL.LSList[132], 100, false);
			this.FWebsList = new TSheetList(this.SheetWebs);
			this.FWebsList.Buttons = TEnumSet.Create();
			this.FWebsList.List.AddListColumn(GKL.LSList[96], 350, false);
			this.FWebsList.List.AddListColumn(GKL.LSList[133], 100, false);
			this.Text = GKL.LSList[34];
		}

		private void _CollectData_AddItem(TGKListView aList, string aPerson, string aData)
		{
			TExtListItem item = aList.AddItem(aPerson, null);
			item.SubItems.Add(aData);
		}

		private void _CollectData_PrepareEvent([In] TfmOrganizer Self, string iName, TGEDCOMCustomEvent ev)
		{
			TGEDCOMAddress addr = ev.Detail.Address;
			if (addr != null)
			{
				string addr_str = addr.Address.Text.Trim();
				if (addr_str != "")
				{
					_CollectData_AddItem(Self.FAdrList.List, iName, addr_str);
				}

				int num = addr.GetPhoneNumbersCount() - 1;
				for (int i = 0; i <= num; i++)
				{
					_CollectData_AddItem(Self.FPhonesList.List, iName, addr.GetPhoneNumber(i));
				}

				int num2 = addr.GetEmailAddressesCount() - 1;
				for (int i = 0; i <= num2; i++)
				{
					_CollectData_AddItem(Self.FMailsList.List, iName, addr.GetEmailAddress(i));
				}

				int num3 = addr.GetWebPagesCount() - 1;
				for (int i = 0; i <= num3; i++)
				{
					_CollectData_AddItem(Self.FWebsList.List, iName, addr.GetWebPage(i));
				}
			}
		}
	}
}
