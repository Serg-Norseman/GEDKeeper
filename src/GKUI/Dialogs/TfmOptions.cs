using System;
using System.Drawing;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI
{
	public partial class TfmOptions : Form, ILocalization
	{
		private TGlobalOptions FOptions;
		private TGlobalOptions.TPersonColumnProps[] FPersonColumns = new TGlobalOptions.TPersonColumnProps[24];

		public TGlobalOptions Options
		{
			get { return this.FOptions; }
			set { this.FOptions = value; }
		}

		private void UpdateColumnsList()
		{
			this.ListPersonColumns.ItemCheck -= new ItemCheckEventHandler(this.ListPersonColumns_ItemCheck);
			this.ListPersonColumns.BeginUpdate();
			try
			{
				this.ListPersonColumns.Items.Clear();
				int i = 0;
				do
				{
					TGlobalOptions.TPersonColumnType pct = this.FPersonColumns[i].colType;
					this.ListPersonColumns.Items.Add(GKL.LSList[(int)TGlobalOptions.PersonColumnsName[(int)pct].Name - 1], this.FPersonColumns[i].colActive);
					i++;
				}
				while (i != 24);
			}
			finally
			{
				this.ListPersonColumns.EndUpdate();
			}
			this.ListPersonColumns.ItemCheck += new ItemCheckEventHandler(this.ListPersonColumns_ItemCheck);
		}

		private void UpdateControls()
		{
			this.PanDefFont.Text = this.FOptions.ChartOptions.DefFont_Name + ", " + this.FOptions.ChartOptions.DefFont_Size.ToString();
		}

		private void UpdateForm()
		{
			TGEDCOMCharacterSet defCharacterSet = this.FOptions.DefCharacterSet;
			if (defCharacterSet != TGEDCOMCharacterSet.csASCII)
			{
				if (defCharacterSet == TGEDCOMCharacterSet.csUTF8)
				{
					this.RButton2.Checked = true;
				}
			}
			else
			{
				this.RButton1.Checked = true;
			}
			TGenEngine.TNameFormat defNameFormat = this.FOptions.DefNameFormat;
			if (defNameFormat != TGenEngine.TNameFormat.nfFNP)
			{
				if (defNameFormat != TGenEngine.TNameFormat.nfF_NP)
				{
					if (defNameFormat == TGenEngine.TNameFormat.nfF_N_P)
					{
						this.RButton7.Checked = true;
					}
				}
				else
				{
					this.RButton6.Checked = true;
				}
			}
			else
			{
				this.RButton5.Checked = true;
			}
			TGenEngine.TDateFormat defDateFormat = this.FOptions.DefDateFormat;
			if (defDateFormat != TGenEngine.TDateFormat.dfDD_MM_YYYY)
			{
				if (defDateFormat == TGenEngine.TDateFormat.dfYYYY_MM_DD)
				{
					this.RButton9.Checked = true;
				}
			}
			else
			{
				this.RButton8.Checked = true;
			}
			this.chkPlacesWithAddress.Checked = this.FOptions.PlacesWithAddress;
			this.chkHighlightUnparented.Checked = this.FOptions.ListPersons_HighlightUnparented;
			this.chkHighlightUnmarried.Checked = this.FOptions.ListPersons_HighlightUnmarried;
			this.chkFamily.Checked = this.FOptions.ChartOptions.FamilyVisible;
			this.chkName.Checked = this.FOptions.ChartOptions.NameVisible;
			this.chkPatronymic.Checked = this.FOptions.ChartOptions.PatronymicVisible;
			this.chkDiffLines.Checked = this.FOptions.ChartOptions.DiffLines;
			this.chkBirthDate.Checked = this.FOptions.ChartOptions.BirthDateVisible;
			this.chkDeathDate.Checked = this.FOptions.ChartOptions.DeathDateVisible;
			this.chkOnlyYears.Checked = this.FOptions.ChartOptions.OnlyYears;
			this.chkKinship.Checked = this.FOptions.ChartOptions.Kinship;
			this.chkSignsVisible.Checked = this.FOptions.ChartOptions.SignsVisible;
			this.chkChildlessExclude.Checked = this.FOptions.ChartOptions.ChildlessExclude;
			this.chkTreeDecorative.Checked = this.FOptions.ChartOptions.Decorative;
			this.chkPortraitsVisible.Checked = this.FOptions.ChartOptions.PortraitsVisible;
			this.PanMaleColor.BackColor = this.FOptions.ChartOptions.MaleColor;
			this.PanFemaleColor.BackColor = this.FOptions.ChartOptions.FemaleColor;
			this.PanUnkSexColor.BackColor = this.FOptions.ChartOptions.UnkSexColor;
			this.PanUnHusbandColor.BackColor = this.FOptions.ChartOptions.UnHusbandColor;
			this.PanUnWifeColor.BackColor = this.FOptions.ChartOptions.UnWifeColor;
			this.chkProxy.Checked = this.FOptions.Proxy.UseProxy;
			this.edProxyServer.Text = this.FOptions.Proxy.Server;
			this.edProxyPort.Text = this.FOptions.Proxy.Port;
			this.edProxyLogin.Text = this.FOptions.Proxy.Login;
			this.edProxyPass.Text = this.FOptions.Proxy.Password;
			this.chkAttributes.Checked = this.FOptions.PedigreeOptions.IncludeAttributes;
			this.chkNotes.Checked = this.FOptions.PedigreeOptions.IncludeNotes;
			this.chkSources.Checked = this.FOptions.PedigreeOptions.IncludeSources;
			TPedigreeOptions.TPedigreeFormat format = this.FOptions.PedigreeOptions.Format;
			if (format != TPedigreeOptions.TPedigreeFormat.pfExcess)
			{
				if (format == TPedigreeOptions.TPedigreeFormat.pfCompact)
				{
					this.RButton11.Checked = true;
				}
			}
			else
			{
				this.RButton10.Checked = true;
			}
			this.chkShowOnStart.Checked = this.FOptions.ShowTips;
			TGlobalOptions.TWorkMode workMode = this.FOptions.WorkMode;
			if (workMode != TGlobalOptions.TWorkMode.wmSimple)
			{
				if (workMode == TGlobalOptions.TWorkMode.wmExpert)
				{
					this.RButton4.Checked = true;
				}
			}
			else
			{
				this.RButton3.Checked = true;
			}
			Array.Copy(this.FOptions.ListPersonsColumns, this.FPersonColumns, 24);
			this.UpdateColumnsList();
			this.UpdateControls();

			this.cbLanguages.Items.Clear();
			this.cbLanguages.Items.Add(new TTaggedComboItem("Русский", 1049));

			int idx = 0;
			int num = this.FOptions.LangsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGlobalOptions.TLangRecord lng_rec = this.FOptions.GetLang(i);
				if (this.FOptions.InterfaceLang == lng_rec.Code)
				{
					idx = i + 1;
				}
				this.cbLanguages.Items.Add(new TTaggedComboItem(lng_rec.Name, (int)lng_rec.Code));
			}
			this.cbLanguages.SelectedIndex = idx;
		}

		private void PanColor_Click(object sender, EventArgs e)
		{
			this.ColorDialog1.Color = (sender as Panel).BackColor;
			if (this.ColorDialog1.ShowDialog() == DialogResult.OK)
			{
				(sender as Panel).BackColor = this.ColorDialog1.Color;
			}
		}

		private void PanDefFont_Click(object sender, EventArgs e)
		{
			if (this.FontDialog1.ShowDialog() == DialogResult.OK)
			{
				this.FOptions.ChartOptions.DefFont_Name = this.FontDialog1.Font.Name;
				this.FOptions.ChartOptions.DefFont_Size = (int)checked((long)Math.Round((double)this.FontDialog1.Font.Size));
			}
			this.UpdateControls();
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmOptions.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}
		
		private void AcceptChanges()
		{
			this.FOptions.ListPersonsColumns = this.FPersonColumns;
			if (this.RButton1.Checked)
			{
				this.FOptions.DefCharacterSet = TGEDCOMCharacterSet.csASCII;
			}
			else
			{
				if (this.RButton2.Checked)
				{
					this.FOptions.DefCharacterSet = TGEDCOMCharacterSet.csUTF8;
				}
			}
			if (this.RButton5.Checked)
			{
				this.FOptions.DefNameFormat = TGenEngine.TNameFormat.nfFNP;
			}
			else
			{
				if (this.RButton6.Checked)
				{
					this.FOptions.DefNameFormat = TGenEngine.TNameFormat.nfF_NP;
				}
				else
				{
					if (this.RButton7.Checked)
					{
						this.FOptions.DefNameFormat = TGenEngine.TNameFormat.nfF_N_P;
					}
				}
			}
			if (this.RButton8.Checked)
			{
				this.FOptions.DefDateFormat = TGenEngine.TDateFormat.dfDD_MM_YYYY;
			}
			else
			{
				if (this.RButton9.Checked)
				{
					this.FOptions.DefDateFormat = TGenEngine.TDateFormat.dfYYYY_MM_DD;
				}
			}
			this.FOptions.PlacesWithAddress = this.chkPlacesWithAddress.Checked;
			this.FOptions.ListPersons_HighlightUnparented = this.chkHighlightUnparented.Checked;
			this.FOptions.ListPersons_HighlightUnmarried = this.chkHighlightUnmarried.Checked;
			this.FOptions.ChartOptions.FamilyVisible = this.chkFamily.Checked;
			this.FOptions.ChartOptions.NameVisible = this.chkName.Checked;
			this.FOptions.ChartOptions.PatronymicVisible = this.chkPatronymic.Checked;
			this.FOptions.ChartOptions.DiffLines = this.chkDiffLines.Checked;
			this.FOptions.ChartOptions.BirthDateVisible = this.chkBirthDate.Checked;
			this.FOptions.ChartOptions.DeathDateVisible = this.chkDeathDate.Checked;
			this.FOptions.ChartOptions.OnlyYears = this.chkOnlyYears.Checked;
			this.FOptions.ChartOptions.Kinship = this.chkKinship.Checked;
			this.FOptions.ChartOptions.SignsVisible = this.chkSignsVisible.Checked;
			this.FOptions.ChartOptions.ChildlessExclude = this.chkChildlessExclude.Checked;
			this.FOptions.ChartOptions.Decorative = this.chkTreeDecorative.Checked;
			this.FOptions.ChartOptions.PortraitsVisible = this.chkPortraitsVisible.Checked;
			this.FOptions.ChartOptions.MaleColor = this.PanMaleColor.BackColor;
			this.FOptions.ChartOptions.FemaleColor = this.PanFemaleColor.BackColor;
			this.FOptions.ChartOptions.UnkSexColor = this.PanUnkSexColor.BackColor;
			this.FOptions.ChartOptions.UnHusbandColor = this.PanUnHusbandColor.BackColor;
			this.FOptions.ChartOptions.UnWifeColor = this.PanUnWifeColor.BackColor;
			this.FOptions.Proxy.UseProxy = this.chkProxy.Checked;
			this.FOptions.Proxy.Server = this.edProxyServer.Text;
			this.FOptions.Proxy.Port = this.edProxyPort.Text;
			this.FOptions.Proxy.Login = this.edProxyLogin.Text;
			this.FOptions.Proxy.Password = this.edProxyPass.Text;
			this.FOptions.PedigreeOptions.IncludeAttributes = this.chkAttributes.Checked;
			this.FOptions.PedigreeOptions.IncludeNotes = this.chkNotes.Checked;
			this.FOptions.PedigreeOptions.IncludeSources = this.chkSources.Checked;
			if (this.RButton10.Checked)
			{
				this.FOptions.PedigreeOptions.Format = TPedigreeOptions.TPedigreeFormat.pfExcess;
			}
			else
			{
				if (this.RButton11.Checked)
				{
					this.FOptions.PedigreeOptions.Format = TPedigreeOptions.TPedigreeFormat.pfCompact;
				}
			}
			this.FOptions.ShowTips = this.chkShowOnStart.Checked;
			if (this.RButton3.Checked)
			{
				this.FOptions.WorkMode = TGlobalOptions.TWorkMode.wmSimple;
			}
			else
			{
				if (this.RButton4.Checked)
				{
					this.FOptions.WorkMode = TGlobalOptions.TWorkMode.wmExpert;
				}
			}

			int code = (this.cbLanguages.Items[this.cbLanguages.SelectedIndex] as TTaggedComboItem).Tag;
			GKUI.TfmGEDKeeper.Instance.LoadLanguage(code);

			base.DialogResult = DialogResult.OK;
		}

		private void btnColumnUp_Click(object sender, EventArgs e)
		{
			int idx = this.ListPersonColumns.SelectedIndex;
			if (idx > 0)
			{
				TGlobalOptions.TPersonColumnProps props = this.FPersonColumns[idx - 1];
				this.FPersonColumns[idx - 1] = this.FPersonColumns[idx];
				this.FPersonColumns[idx] = props;
				this.UpdateColumnsList();
				this.ListPersonColumns.SelectedIndex = idx - 1;
			}
		}

		private void btnColumnDown_Click(object sender, EventArgs e)
		{
			int idx = this.ListPersonColumns.SelectedIndex;
			if (idx >= 0 && idx < 23)
			{
				TGlobalOptions.TPersonColumnProps props = this.FPersonColumns[idx + 1];
				this.FPersonColumns[idx + 1] = this.FPersonColumns[idx];
				this.FPersonColumns[idx] = props;
				this.UpdateColumnsList();
				this.ListPersonColumns.SelectedIndex = idx + 1;
			}
		}

		private void btnDefList_Click(object sender, EventArgs e)
		{
			Array.Copy(TGlobalOptions.DefPersonColumns, this.FPersonColumns, 24);
			this.UpdateColumnsList();
		}

		private void ListPersonColumns_ItemCheck(object sender, ItemCheckEventArgs e)
		{
			bool cs = (e.NewValue == CheckState.Checked);
			this.FPersonColumns[e.Index].colActive = cs;
		}

		public TfmOptions()
		{
			this.InitializeComponent();
			this.FOptions = GKUI.TfmGEDKeeper.Instance.Options;
			(this as ILocalization).SetLang();
			this.UpdateForm();
		}

		void ILocalization.SetLang()
		{
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[39];
			this.SheetCommon.Text = GKL.LSList[144];
			this.SheetView.Text = GKL.LSList[249];
			this.SheetTree.Text = GKL.LSList[250];
			this.SheetPedigree.Text = GKL.LSList[251];
			this.rgCode.Text = GKL.LSList[252];
			this.rgEditMode.Text = GKL.LSList[253];
			this.RButton3.Text = GKL.LSList[254];
			this.RButton4.Text = GKL.LSList[255];
			this.GroupBox4.Text = GKL.LSList[256];
			this.chkProxy.Text = GKL.LSList[257];
			this.Label1.Text = GKL.LSList[258];
			this.Label2.Text = GKL.LSList[259];
			this.Label3.Text = GKL.LSList[260];
			this.Label4.Text = GKL.LSList[261];
			this.GroupBox7.Text = GKL.LSList[262];
			this.chkShowOnStart.Text = GKL.LSList[263];
			this.Label6.Text = GKL.LSList[264];
			this.SheetViewCommon.Text = GKL.LSList[265];
			this.SheetViewPersons.Text = GKL.LSList[266];
			this.rgFNPFormat.Text = GKL.LSList[267];
			this.RButton5.Text = GKL.LSList[268];
			this.RButton6.Text = GKL.LSList[269];
			this.RButton7.Text = GKL.LSList[270];
			this.rgDateFormat.Text = GKL.LSList[271];
			this.chkPlacesWithAddress.Text = GKL.LSList[272];
			this.chkHighlightUnparented.Text = GKL.LSList[273];
			this.chkHighlightUnmarried.Text = GKL.LSList[274];
			this.btnDefList.Text = GKL.LSList[275];
			this.GroupBox1.Text = GKL.LSList[276];
			this.chkFamily.Text = GKL.LSList[84];
			this.chkName.Text = GKL.LSList[85];
			this.chkPatronymic.Text = GKL.LSList[86];
			this.chkDiffLines.Text = GKL.LSList[277];
			this.chkBirthDate.Text = GKL.LSList[122];
			this.chkDeathDate.Text = GKL.LSList[123];
			this.chkOnlyYears.Text = GKL.LSList[278];
			this.chkKinship.Text = GKL.LSList[279];
			this.chkSignsVisible.Text = GKL.LSList[280];
			this.chkTreeDecorative.Text = GKL.LSList[281];
			this.chkPortraitsVisible.Text = GKL.LSList[282];
			this.chkChildlessExclude.Text = GKL.LSList[283];
			this.GroupBox2.Text = GKL.LSList[284];
			this.PanMaleColor.Text = GKL.LSList[285];
			this.PanFemaleColor.Text = GKL.LSList[286];
			this.PanUnkSexColor.Text = GKL.LSList[287];
			this.PanUnHusbandColor.Text = GKL.LSList[288];
			this.PanUnWifeColor.Text = GKL.LSList[289];
			this.Label5.Text = GKL.LSList[290];
			this.GroupBox5.Text = GKL.LSList[291];
			this.chkAttributes.Text = GKL.LSList[292];
			this.chkNotes.Text = GKL.LSList[293];
			this.chkSources.Text = GKL.LSList[294];
			this.EditPedigreeFormat.Text = GKL.LSList[295];
			this.RButton10.Text = GKL.LSList[296];
			this.RButton11.Text = GKL.LSList[297];
		}
	}
}
