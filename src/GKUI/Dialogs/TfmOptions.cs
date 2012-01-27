using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Settings;
using GKSys;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmOptions : Form, ILocalization
	{
		private TGlobalOptions FOptions;
		private TIndividualListColumns FTempColumns = new TIndividualListColumns();

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
				for (int i = 0; i < FTempColumns.Count; i++)
				{
					TPersonColumnType pct = FTempColumns[i].colType;
					this.ListPersonColumns.Items.Add(LangMan.LSList[(int)TGlobalOptions.PersonColumnsName[(int)pct].Name - 1], FTempColumns[i].colActive);
				}
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
			this.chkRevisionsBackup.Checked = this.FOptions.RevisionsBackup;

			this.FOptions.IndividualListColumns.CopyTo(FTempColumns);
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
			this.FTempColumns.CopyTo(this.FOptions.IndividualListColumns);

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
			this.FOptions.RevisionsBackup = this.chkRevisionsBackup.Checked;

			int code = (this.cbLanguages.Items[this.cbLanguages.SelectedIndex] as TTaggedComboItem).Tag;
			GKUI.TfmGEDKeeper.Instance.LoadLanguage(code);

			base.DialogResult = DialogResult.OK;
		}

		private void btnColumnUp_Click(object sender, EventArgs e)
		{
			int idx = this.ListPersonColumns.SelectedIndex;
			if (idx > 0)
			{
				TPersonColumnProps props = this.FTempColumns[idx - 1];
				this.FTempColumns[idx - 1] = this.FTempColumns[idx];
				this.FTempColumns[idx] = props;
				this.UpdateColumnsList();
				this.ListPersonColumns.SelectedIndex = idx - 1;
			}
		}

		private void btnColumnDown_Click(object sender, EventArgs e)
		{
			int idx = this.ListPersonColumns.SelectedIndex;
			if (idx >= 0 && idx < 24)
			{
				TPersonColumnProps props = this.FTempColumns[idx + 1];
				this.FTempColumns[idx + 1] = this.FTempColumns[idx];
				this.FTempColumns[idx] = props;
				this.UpdateColumnsList();
				this.ListPersonColumns.SelectedIndex = idx + 1;
			}
		}

		private void btnDefList_Click(object sender, EventArgs e)
		{
			this.FTempColumns.SetDefaults();
			this.UpdateColumnsList();
		}

		private void ListPersonColumns_ItemCheck(object sender, ItemCheckEventArgs e)
		{
			bool cs = (e.NewValue == CheckState.Checked);
			TPersonColumnProps props = this.FTempColumns[e.Index];
			props.colActive = cs;
			this.FTempColumns[e.Index] = props;
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
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[39];
			this.SheetCommon.Text = LangMan.LSList[144];
			this.SheetView.Text = LangMan.LSList[249];
			this.SheetTree.Text = LangMan.LSList[250];
			this.SheetPedigree.Text = LangMan.LSList[251];
			this.rgCode.Text = LangMan.LSList[252];

			this.GroupBox4.Text = LangMan.LSList[256];
			this.chkProxy.Text = LangMan.LSList[257];
			this.Label1.Text = LangMan.LSList[258];
			this.Label2.Text = LangMan.LSList[259];
			this.Label3.Text = LangMan.LSList[260];
			this.Label4.Text = LangMan.LSList[261];

			this.GroupBox7.Text = LangMan.LS(LSID.LSID_Other);

			this.chkShowOnStart.Text = LangMan.LSList[263];
			this.Label6.Text = LangMan.LSList[264];
			this.SheetViewCommon.Text = LangMan.LSList[265];
			this.SheetViewPersons.Text = LangMan.LSList[266];
			this.rgFNPFormat.Text = LangMan.LSList[267];
			this.RButton5.Text = LangMan.LSList[268];
			this.RButton6.Text = LangMan.LSList[269];
			this.RButton7.Text = LangMan.LSList[270];
			this.rgDateFormat.Text = LangMan.LSList[271];
			this.chkPlacesWithAddress.Text = LangMan.LSList[272];
			this.chkHighlightUnparented.Text = LangMan.LSList[273];
			this.chkHighlightUnmarried.Text = LangMan.LSList[274];
			this.btnDefList.Text = LangMan.LSList[275];
			this.GroupBox1.Text = LangMan.LSList[276];
			this.chkFamily.Text = LangMan.LSList[84];
			this.chkName.Text = LangMan.LSList[85];
			this.chkPatronymic.Text = LangMan.LSList[86];
			this.chkDiffLines.Text = LangMan.LSList[277];
			this.chkBirthDate.Text = LangMan.LSList[122];
			this.chkDeathDate.Text = LangMan.LSList[123];
			this.chkOnlyYears.Text = LangMan.LSList[278];
			this.chkKinship.Text = LangMan.LSList[279];
			this.chkSignsVisible.Text = LangMan.LSList[280];
			this.chkTreeDecorative.Text = LangMan.LSList[281];
			this.chkPortraitsVisible.Text = LangMan.LSList[282];
			this.chkChildlessExclude.Text = LangMan.LSList[283];
			this.GroupBox2.Text = LangMan.LSList[284];
			this.PanMaleColor.Text = LangMan.LSList[285];
			this.PanFemaleColor.Text = LangMan.LSList[286];
			this.PanUnkSexColor.Text = LangMan.LSList[287];
			this.PanUnHusbandColor.Text = LangMan.LSList[288];
			this.PanUnWifeColor.Text = LangMan.LSList[289];
			this.Label5.Text = LangMan.LSList[290];
			this.GroupBox5.Text = LangMan.LSList[291];
			this.chkAttributes.Text = LangMan.LSList[292];
			this.chkNotes.Text = LangMan.LSList[293];
			this.chkSources.Text = LangMan.LSList[294];
			this.EditPedigreeFormat.Text = LangMan.LSList[295];
			this.RButton10.Text = LangMan.LSList[296];
			this.RButton11.Text = LangMan.LSList[297];
			this.chkRevisionsBackup.Text = LangMan.LS(LSID.LSID_RevisionsBackup);
		}
	}
}
