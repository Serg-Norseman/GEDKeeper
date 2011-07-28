using GedCom551;
using GKCore;
using GKUI.Controls;
using GKUI.Lists;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Resources;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmAddressEdit : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private TabControl PageAddrData;
		private TabPage SheetPhones;
		private TabPage SheetEmails;
		private TabPage SheetCommon;
		private TabPage SheetWebPages;
		private Label Label1;
		private Label Label2;
		private Label Label3;
		private Label Label4;
		private Label Label5;
		private TextBox edCountry;
		private TextBox edState;
		private TextBox edCity;
		private TextBox edPostalCode;
		private TextBox edAddress;
		private TGEDCOMAddress FAddress;
		private TSheetList FPhonesList;
		private TSheetList FMailsList;
		private TSheetList FWebsList;

		[Browsable(false)]
		public TGEDCOMAddress Address
		{
			get
			{
				return this.FAddress;
			}
			set
			{
				this.SetAddress(value);
			}
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
			int arg_24_0 = 0;
			int num = this.FAddress.GetPhoneNumbersCount() - 1;
			int i = arg_24_0;
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
			int arg_7E_0 = 0;
			int num2 = this.FAddress.GetEmailAddressesCount() - 1;
			i = arg_7E_0;
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
			int arg_D8_0 = 0;
			int num3 = this.FAddress.GetWebPagesCount() - 1;
			i = arg_D8_0;
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
		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmAddressEdit));
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.PageAddrData = new TabControl();
			this.SheetCommon = new TabPage();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label3 = new Label();
			this.Label4 = new Label();
			this.Label5 = new Label();
			this.edCountry = new TextBox();
			this.edState = new TextBox();
			this.edCity = new TextBox();
			this.edPostalCode = new TextBox();
			this.edAddress = new TextBox();
			this.SheetPhones = new TabPage();
			this.SheetEmails = new TabPage();
			this.SheetWebPages = new TabPage();
			this.PageAddrData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			base.SuspendLayout();
			this.btnAccept.Image = (resources.GetObject("btnAccept.Image") as Image);
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(232, 280);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Image = (resources.GetObject("btnCancel.Image") as Image);
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(320, 280);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.PageAddrData.Controls.Add(this.SheetCommon);
			this.PageAddrData.Controls.Add(this.SheetPhones);
			this.PageAddrData.Controls.Add(this.SheetEmails);
			this.PageAddrData.Controls.Add(this.SheetWebPages);
			this.PageAddrData.Dock = DockStyle.Top;
			this.PageAddrData.Location = new Point(0, 0);
			this.PageAddrData.Name = "PageAddrData";
			this.PageAddrData.SelectedIndex = 0;
			this.PageAddrData.Size = new Size(409, 264);
			this.PageAddrData.TabIndex = 0;
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.Label4);
			this.SheetCommon.Controls.Add(this.Label5);
			this.SheetCommon.Controls.Add(this.edCountry);
			this.SheetCommon.Controls.Add(this.edState);
			this.SheetCommon.Controls.Add(this.edCity);
			this.SheetCommon.Controls.Add(this.edPostalCode);
			this.SheetCommon.Controls.Add(this.edAddress);
			this.SheetCommon.Location = new Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new Size(401, 238);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Адрес";
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(45, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Страна";
			this.Label2.Location = new Point(216, 8);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(80, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Штат/Область";
			this.Label3.Location = new Point(8, 56);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(35, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Город";
			this.Label4.Location = new Point(216, 56);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(80, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Почтовый код";
			this.Label5.Location = new Point(8, 104);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(35, 13);
			this.Label5.TabIndex = 4;
			this.Label5.Text = "Адрес";
			this.edCountry.Location = new Point(8, 24);
			this.edCountry.Name = "edCountry";
			this.edCountry.Size = new Size(201, 21);
			this.edCountry.TabIndex = 0;
			this.edCountry.Text = "";
			this.edState.Location = new Point(216, 24);
			this.edState.Name = "edState";
			this.edState.Size = new Size(177, 21);
			this.edState.TabIndex = 1;
			this.edState.Text = "";
			this.edCity.Location = new Point(8, 72);
			this.edCity.Name = "edCity";
			this.edCity.Size = new Size(201, 21);
			this.edCity.TabIndex = 2;
			this.edCity.Text = "";
			this.edPostalCode.Location = new Point(216, 72);
			this.edPostalCode.Name = "edPostalCode";
			this.edPostalCode.Size = new Size(177, 21);
			this.edPostalCode.TabIndex = 3;
			this.edPostalCode.Text = "";
			this.edAddress.Location = new Point(8, 120);
			this.edAddress.Name = "edAddress";
			this.edAddress.Size = new Size(385, 21);
			this.edAddress.TabIndex = 4;
			this.edAddress.Text = "";
			this.SheetPhones.Location = new Point(4, 22);
			this.SheetPhones.Name = "SheetPhones";
			this.SheetPhones.Size = new Size(401, 238);
			this.SheetPhones.TabIndex = 1;
			this.SheetPhones.Text = "Телефоны";
			this.SheetEmails.Location = new Point(4, 22);
			this.SheetEmails.Name = "SheetEmails";
			this.SheetEmails.Size = new Size(401, 238);
			this.SheetEmails.TabIndex = 2;
			this.SheetEmails.Text = "Эл. почта";
			this.SheetWebPages.Location = new Point(4, 22);
			this.SheetWebPages.Name = "SheetWebPages";
			this.SheetWebPages.Size = new Size(401, 238);
			this.SheetWebPages.TabIndex = 3;
			this.SheetWebPages.Text = "Веб-страницы";
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(409, 313);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.PageAddrData);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmAddressEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Адрес";
			this.PageAddrData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			this.FAddress.AddressCountry = this.edCountry.Text;
			this.FAddress.AddressState = this.edState.Text;
			this.FAddress.AddressCity = this.edCity.Text;
			this.FAddress.AddressPostalCode = this.edPostalCode.Text;
			TGenEngine.SetAddressValue(this.FAddress, this.edAddress.Text);
			base.DialogResult = DialogResult.OK;
		}
		public TfmAddressEdit()
		{
			this.InitializeComponent();
			this.FPhonesList = new TSheetList(this.SheetPhones);
			this.FPhonesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FPhonesList.List.AddListColumn(GKL.LSList[131], 350, false);
			this.FMailsList = new TSheetList(this.SheetEmails);
			this.FMailsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FMailsList.List.AddListColumn(GKL.LSList[132], 350, false);
			this.FWebsList = new TSheetList(this.SheetWebPages);
			this.FWebsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
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
			return InputBox.Query(aTitle, GKL.LSList[202], ref aValue) && BDSSystem.WStrCmp(aValue.Trim(), "") != 0;
		}
	}
}
