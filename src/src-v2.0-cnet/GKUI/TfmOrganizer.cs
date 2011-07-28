using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using GKUI.Lists;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmOrganizer : Form
	{
		private TabControl PageControl1;
		private TabPage SheetAddresses;
		private TabPage SheetTelephones;
		private TabPage SheetEMails;
		private TabPage SheetWebs;
		private TSheetList FAdrList;
		private TSheetList FPhonesList;
		private TSheetList FMailsList;
		private TSheetList FWebsList;
		private TfmBase FBase;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		private void CollectData()
		{
			this.FAdrList.List.Items.Clear();
			this.FPhonesList.List.Items.Clear();
			this.FMailsList.List.Items.Clear();
			this.FWebsList.List.Items.Clear();
			int arg_69_0 = 0;
			int num = this.Base.Tree.RecordsCount - 1;
			int i = arg_69_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMRecord rec = this.Base.Tree.GetRecord(i);
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
						string nm = TGenEngine.GetNameStr(i_rec, true, false);
						int arg_B2_0 = 0;
						int num2 = i_rec.IndividualEventsCount - 1;
						int j = arg_B2_0;
						if (num2 >= j)
						{
							num2++;
							do
							{
								TfmOrganizer._CollectData_PrepareEvent(this, nm, i_rec.GetIndividualEvent(j));
								j++;
							}
							while (j != num2);
						}
					}
					i++;
				}
				while (i != num);
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
		private void InitializeComponent()
		{
			this.PageControl1 = new TabControl();
			this.SheetAddresses = new TabPage();
			this.SheetTelephones = new TabPage();
			this.SheetEMails = new TabPage();
			this.SheetWebs = new TabPage();
			this.PageControl1.SuspendLayout();
			base.SuspendLayout();
			this.PageControl1.Controls.Add(this.SheetAddresses);
			this.PageControl1.Controls.Add(this.SheetTelephones);
			this.PageControl1.Controls.Add(this.SheetEMails);
			this.PageControl1.Controls.Add(this.SheetWebs);
			this.PageControl1.Dock = DockStyle.Fill;
			this.PageControl1.Location = new Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new Size(736, 476);
			this.PageControl1.TabIndex = 0;
			this.SheetAddresses.Location = new Point(4, 22);
			this.SheetAddresses.Name = "SheetAddresses";
			this.SheetAddresses.Size = new Size(728, 450);
			this.SheetAddresses.TabIndex = 0;
			this.SheetAddresses.Text = "Адреса";
			this.SheetTelephones.Location = new Point(4, 22);
			this.SheetTelephones.Name = "SheetTelephones";
			this.SheetTelephones.Size = new Size(728, 496);
			this.SheetTelephones.TabIndex = 1;
			this.SheetTelephones.Text = "Телефоны";
			this.SheetEMails.Location = new Point(4, 22);
			this.SheetEMails.Name = "SheetEMails";
			this.SheetEMails.Size = new Size(728, 496);
			this.SheetEMails.TabIndex = 2;
			this.SheetEMails.Text = "Почта";
			this.SheetWebs.Location = new Point(4, 22);
			this.SheetWebs.Name = "SheetWebs";
			this.SheetWebs.Size = new Size(728, 496);
			this.SheetWebs.TabIndex = 3;
			this.SheetWebs.Text = "Сайты";
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(736, 476);
			base.Controls.Add(this.PageControl1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmOrganizer";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Органайзер";
			base.Load += new EventHandler(this.TfmOrganizer_Load);
			this.PageControl1.ResumeLayout(false);
			base.ResumeLayout(false);
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

		private static void _CollectData_AddItem(TGKListView aList, string aPerson, string aData)
		{
			TExtListItem item = aList.AddItem(aPerson, null);
			item.SubItems.Add(aData);
		}

		private static void _CollectData_PrepareEvent([In] TfmOrganizer Self, string iName, TGEDCOMCustomEvent ev)
		{
			TGEDCOMAddress addr = ev.Detail.Address;
			if (addr != null)
			{
				string addr_str = addr.Address.Text.Trim();
				if (BDSSystem.WStrCmp(addr_str, "") != 0)
				{
					TfmOrganizer._CollectData_AddItem(Self.FAdrList.List, iName, addr_str);
				}
				int arg_4F_0 = 0;
				int num = addr.GetPhoneNumbersCount() - 1;
				int i = arg_4F_0;
				if (num >= i)
				{
					num++;
					do
					{
						TfmOrganizer._CollectData_AddItem(Self.FPhonesList.List, iName, addr.GetPhoneNumber(i));
						i++;
					}
					while (i != num);
				}
				int arg_82_0 = 0;
				int num2 = addr.GetEmailAddressesCount() - 1;
				i = arg_82_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TfmOrganizer._CollectData_AddItem(Self.FMailsList.List, iName, addr.GetEmailAddress(i));
						i++;
					}
					while (i != num2);
				}
				int arg_B6_0 = 0;
				int num3 = addr.GetWebPagesCount() - 1;
				i = arg_B6_0;
				if (num3 >= i)
				{
					num3++;
					do
					{
						TfmOrganizer._CollectData_AddItem(Self.FWebsList.List, iName, addr.GetWebPage(i));
						i++;
					}
					while (i != num3);
				}
			}
		}
	}
}
