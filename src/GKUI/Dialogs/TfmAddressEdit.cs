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
	public partial class TfmAddressEdit : Form
	{
		private TGEDCOMAddress FAddress;
		private TSheetList FPhonesList;
		private TSheetList FMailsList;
		private TSheetList FWebsList;

		public TGEDCOMAddress Address
		{
			get { return this.FAddress; }
			set { this.SetAddress(value); }
		}

		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			int Index = -1;

			if (Action >= TGenEngine.TRecAction.raEdit && Action < TGenEngine.TRecAction.raJump)
			{
				IntPtr ptr = (IntPtr)ItemData;
				Index = (int)ptr - 1;
			}
			if (object.Equals(Sender, this.FPhonesList))
			{
				if (Action != TGenEngine.TRecAction.raAdd)
				{
					if (Action != TGenEngine.TRecAction.raEdit)
					{
						if (Action == TGenEngine.TRecAction.raDelete)
						{
							if (Index < 0)
							{
								return;
							}
							this.FAddress.DeletePhoneNumber(Index);
						}
					}
					else
					{
						if (Index < 0)
						{
							return;
						}
						string val = this.FAddress.GetPhoneNumber(Index);
						if (TfmAddressEdit.GetInput(GKL.LSList[131], ref val))
						{
							this.FAddress.SetPhoneNumber(Index, val);
						}
					}
				}
				else
				{
					string val = "";
					if (TfmAddressEdit.GetInput(GKL.LSList[131], ref val))
					{
						this.FAddress.SetPhoneNumber(this.FAddress.GetPhoneNumbersCount(), val);
					}
				}
			}
			else
			{
				if (object.Equals(Sender, this.FMailsList))
				{
					if (Action != TGenEngine.TRecAction.raAdd)
					{
						if (Action != TGenEngine.TRecAction.raEdit)
						{
							if (Action == TGenEngine.TRecAction.raDelete)
							{
								if (Index < 0)
								{
									return;
								}
								this.FAddress.DeleteEmail(Index);
							}
						}
						else
						{
							if (Index < 0)
							{
								return;
							}
							string val = this.FAddress.GetEmailAddress(Index);
							if (TfmAddressEdit.GetInput(GKL.LSList[132], ref val))
							{
								this.FAddress.SetEmailAddress(Index, val);
							}
						}
					}
					else
					{
						string val = "";
						if (TfmAddressEdit.GetInput(GKL.LSList[132], ref val))
						{
							this.FAddress.SetEmailAddress(this.FAddress.GetEmailAddressesCount(), val);
						}
					}
				}
				else
				{
					if (object.Equals(Sender, this.FWebsList))
					{
						if (Action != TGenEngine.TRecAction.raAdd)
						{
							if (Action != TGenEngine.TRecAction.raEdit)
							{
								if (Action == TGenEngine.TRecAction.raDelete)
								{
									if (Index < 0)
									{
										return;
									}
									this.FAddress.DeleteWebPage(Index);
								}
							}
							else
							{
								if (Index < 0)
								{
									return;
								}
								string val = this.FAddress.GetWebPage(Index);
								if (TfmAddressEdit.GetInput(GKL.LSList[133], ref val))
								{
									this.FAddress.SetWebPage(Index, val);
								}
							}
						}
						else
						{
							string val = "";
							if (TfmAddressEdit.GetInput(GKL.LSList[133], ref val))
							{
								this.FAddress.SetWebPage(this.FAddress.GetWebPagesCount(), val);
							}
						}
					}
				}
			}
			this.UpdateLists();
		}

		private void SetAddress([In] TGEDCOMAddress Value)
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
			int num = this.FAddress.GetPhoneNumbersCount() - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					this.FPhonesList.List.AddItem(this.FAddress.GetPhoneNumber(i), i + 1);
					i++;
				}
				while (i != num);
			}

			this.FMailsList.List.Items.Clear();
			int num2 = this.FAddress.GetEmailAddressesCount() - 1;
			i = 0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					this.FMailsList.List.AddItem(this.FAddress.GetEmailAddress(i), i + 1);
					i++;
				}
				while (i != num2);
			}

			this.FWebsList.List.Items.Clear();
			int num3 = this.FAddress.GetWebPagesCount() - 1;
			i = 0;
			if (num3 >= i)
			{
				num3++;
				do
				{
					this.FWebsList.List.AddItem(this.FAddress.GetWebPage(i), i + 1);
					i++;
				}
				while (i != num3);
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
				TGenEngine.SetAddressValue(this.FAddress, this.edAddress.Text);
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

			this.FPhonesList = new TSheetList(this.SheetPhones);
			this.FPhonesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FPhonesList.List.AddListColumn(GKL.LSList[131], 350, false);

			this.FMailsList = new TSheetList(this.SheetEmails);
			this.FMailsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FMailsList.List.AddListColumn(GKL.LSList[132], 350, false);

			this.FWebsList = new TSheetList(this.SheetWebPages);
			this.FWebsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FWebsList.List.AddListColumn(GKL.LSList[133], 350, false);

			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[82];
			this.SheetCommon.Text = GKL.LSList[82];
			this.Label1.Text = GKL.LSList[195];
			this.Label2.Text = GKL.LSList[196];
			this.Label3.Text = GKL.LSList[197];
			this.Label4.Text = GKL.LSList[198];
			this.Label5.Text = GKL.LSList[82];
			this.SheetPhones.Text = GKL.LSList[199];
			this.SheetEmails.Text = GKL.LSList[200];
			this.SheetWebPages.Text = GKL.LSList[201];
		}

		private static bool GetInput(string aTitle, ref string aValue)
		{
			return InputBox.Query(aTitle, GKL.LSList[202], ref aValue) && aValue.Trim() != "";
		}
	}
}
