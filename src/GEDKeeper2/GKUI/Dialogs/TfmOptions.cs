using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Dialogs
{
	public enum OptionsPage
	{
		opCommon, opTreeChart, opAncestorsCircle, opInterface, opPedigree
	}

    public sealed partial class TfmOptions : Form, ILocalization
	{
		private readonly IHost fHost;
		private GlobalOptions fOptions;
		private readonly TIndividualListColumns fTempColumns;

		public GlobalOptions Options
		{
			get { return this.fOptions; }
			set { this.fOptions = value; }
		}

        public TfmOptions(IHost aHost)
        {
            this.InitializeComponent();

            this.fHost = aHost;
            this.fOptions = TfmGEDKeeper.Instance.Options;
            this.fTempColumns = new TIndividualListColumns();

            (this as ILocalization).SetLang();
            this.UpdateForm();
        }

        private void UpdateColumnsList()
		{
			this.ListPersonColumns.ItemCheck -= this.ListPersonColumns_ItemCheck;
			this.ListPersonColumns.BeginUpdate();
			try
			{
				this.ListPersonColumns.Items.Clear();
				for (int i = 0; i < fTempColumns.Count; i++)
				{
					TPersonColumnType pct = (TPersonColumnType)fTempColumns[i].colType;
					string colName = LangMan.LS(GlobalOptions.PersonColumnsName[(int) pct].Name);
                    this.ListPersonColumns.Items.Add(colName, fTempColumns[i].colActive);
				}
			}
			finally
			{
				this.ListPersonColumns.EndUpdate();
			}
			this.ListPersonColumns.ItemCheck += this.ListPersonColumns_ItemCheck;
		}

		private void UpdateControls()
		{
			this.lblChartFont.Text = this.fOptions.ChartOptions.DefFont_Name + ", " + this.fOptions.ChartOptions.DefFont_Size.ToString();
		}

	    private void UpdateLangs()
	    {
            this.cbLanguages.Items.Clear();
            this.cbLanguages.Items.Add(new GKComboItem("Русский", 1049));

            int idx = 0;
            int num = this.fOptions.GetLangsCount() - 1;
            for (int i = 0; i <= num; i++)
            {
                GlobalOptions.TLangRecord lngRec = this.fOptions.GetLang(i);
                if (this.fOptions.InterfaceLang == lngRec.Code)
                {
                    idx = i + 1;
                }
                this.cbLanguages.Items.Add(new GKComboItem(lngRec.Name, (int)lngRec.Code));
            }
            this.cbLanguages.SelectedIndex = idx;
        }

		private void UpdateForm()
		{
            switch (this.fOptions.DefCharacterSet)
            {
                case TGEDCOMCharacterSet.csASCII:
                    this.RButton1.Checked = true;
                    break;
                case TGEDCOMCharacterSet.csUTF8:
                    this.RButton2.Checked = true;
                    break;
            }

            switch (this.fOptions.DefNameFormat)
            {
                case NameFormat.nfFNP:
                    this.RButton5.Checked = true;
                    break;
                case NameFormat.nfF_NP:
                    this.RButton6.Checked = true;
                    break;
                case NameFormat.nfF_N_P:
                    this.RButton7.Checked = true;
                    break;
            }

            switch (this.fOptions.DefDateFormat)
            {
                case DateFormat.dfDD_MM_YYYY:
                    this.RButton8.Checked = true;
                    break;
                case DateFormat.dfYYYY_MM_DD:
                    this.RButton9.Checked = true;
                    break;
            }

			this.chkPlacesWithAddress.Checked = this.fOptions.PlacesWithAddress;
			this.chkHighlightUnparented.Checked = this.fOptions.ListPersons_HighlightUnparented;
			this.chkHighlightUnmarried.Checked = this.fOptions.ListPersons_HighlightUnmarried;
			this.chkFamily.Checked = this.fOptions.ChartOptions.FamilyVisible;
			this.chkName.Checked = this.fOptions.ChartOptions.NameVisible;
			this.chkPatronymic.Checked = this.fOptions.ChartOptions.PatronymicVisible;
			this.chkDiffLines.Checked = this.fOptions.ChartOptions.DiffLines;
			this.chkBirthDate.Checked = this.fOptions.ChartOptions.BirthDateVisible;
			this.chkDeathDate.Checked = this.fOptions.ChartOptions.DeathDateVisible;
			this.chkOnlyYears.Checked = this.fOptions.ChartOptions.OnlyYears;
			this.chkKinship.Checked = this.fOptions.ChartOptions.Kinship;
			this.chkSignsVisible.Checked = this.fOptions.ChartOptions.SignsVisible;
			this.chkChildlessExclude.Checked = this.fOptions.ChartOptions.ChildlessExclude;
			this.chkTreeDecorative.Checked = this.fOptions.ChartOptions.Decorative;
			this.chkPortraitsVisible.Checked = this.fOptions.ChartOptions.PortraitsVisible;
			this.PanMaleColor.BackColor = this.fOptions.ChartOptions.MaleColor;
			this.PanFemaleColor.BackColor = this.fOptions.ChartOptions.FemaleColor;
			this.PanUnkSexColor.BackColor = this.fOptions.ChartOptions.UnkSexColor;
			this.PanUnHusbandColor.BackColor = this.fOptions.ChartOptions.UnHusbandColor;
			this.PanUnWifeColor.BackColor = this.fOptions.ChartOptions.UnWifeColor;
			this.chkProxy.Checked = this.fOptions.Proxy.UseProxy;
			this.edProxyServer.Text = this.fOptions.Proxy.Server;
			this.edProxyPort.Text = this.fOptions.Proxy.Port;
			this.edProxyLogin.Text = this.fOptions.Proxy.Login;
			this.edProxyPass.Text = this.fOptions.Proxy.Password;
			this.chkAttributes.Checked = this.fOptions.PedigreeOptions.IncludeAttributes;
			this.chkNotes.Checked = this.fOptions.PedigreeOptions.IncludeNotes;
			this.chkSources.Checked = this.fOptions.PedigreeOptions.IncludeSources;

            switch (this.fOptions.PedigreeOptions.Format)
            {
                case PedigreeOptions.PedigreeFormat.pfExcess:
                    this.RButton10.Checked = true;
                    break;
                case PedigreeOptions.PedigreeFormat.pfCompact:
                    this.RButton11.Checked = true;
                    break;
            }

			this.chkShowOnStart.Checked = this.fOptions.ShowTips;
			this.chkRevisionsBackup.Checked = this.fOptions.RevisionsBackup;

			this.fOptions.IndividualListColumns.CopyTo(fTempColumns);
			this.UpdateColumnsList();

			this.UpdateControls();
		    this.UpdateLangs();
		    
		    this.UpdatePlugins();
		}

		private void UpdatePlugins()
		{
			this.lvPlugins.Items.Clear();
			
			foreach (IPlugin plugin in TfmGEDKeeper.Instance.Plugins)
			{
				PluginInfo pInfo = TfmGEDKeeper.Instance.GetPluginAttributes(plugin);
				
				ListViewItem item = this.lvPlugins.Items.Add(pInfo.Title);
				item.SubItems.Add(pInfo.Version);
				item.SubItems.Add(pInfo.Copyright);
				item.SubItems.Add(pInfo.Description);
			}
		}

		private void PanColor_Click(object sender, EventArgs e)
		{
            Panel pan = (sender as Panel);

            this.ColorDialog1.Color = pan.BackColor;
            if (this.ColorDialog1.ShowDialog() == DialogResult.OK)
            {
                pan.BackColor = this.ColorDialog1.Color;
            }
		}

		private void PanDefFont_Click(object sender, EventArgs e)
		{
			TreeChartOptions chartOptions = this.fOptions.ChartOptions;

			this.FontDialog1.Font = new System.Drawing.Font(chartOptions.DefFont_Name, chartOptions.DefFont_Size);

			if (this.FontDialog1.ShowDialog() == DialogResult.OK)
			{
				chartOptions.DefFont_Name = this.FontDialog1.Font.Name;
				chartOptions.DefFont_Size = (int)checked((long)Math.Round((double)this.FontDialog1.Font.Size));
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
			catch (Exception ex)
			{
				this.fHost.LogWrite("TfmOptions.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void AcceptChanges()
		{
			this.fTempColumns.CopyTo(this.fOptions.IndividualListColumns);

			if (this.RButton1.Checked)
			{
				this.fOptions.DefCharacterSet = TGEDCOMCharacterSet.csASCII;
			}
			else if (this.RButton2.Checked)
			{
				this.fOptions.DefCharacterSet = TGEDCOMCharacterSet.csUTF8;
			}

			if (this.RButton5.Checked)
			{
				this.fOptions.DefNameFormat = NameFormat.nfFNP;
			}
			else if (this.RButton6.Checked)
			{
				this.fOptions.DefNameFormat = NameFormat.nfF_NP;
			}
			else if (this.RButton7.Checked)
			{
				this.fOptions.DefNameFormat = NameFormat.nfF_N_P;
			}

			if (this.RButton8.Checked)
			{
				this.fOptions.DefDateFormat = DateFormat.dfDD_MM_YYYY;
			}
			else if (this.RButton9.Checked)
			{
				this.fOptions.DefDateFormat = DateFormat.dfYYYY_MM_DD;
			}

			this.fOptions.PlacesWithAddress = this.chkPlacesWithAddress.Checked;
			this.fOptions.ListPersons_HighlightUnparented = this.chkHighlightUnparented.Checked;
			this.fOptions.ListPersons_HighlightUnmarried = this.chkHighlightUnmarried.Checked;
			this.fOptions.ChartOptions.FamilyVisible = this.chkFamily.Checked;
			this.fOptions.ChartOptions.NameVisible = this.chkName.Checked;
			this.fOptions.ChartOptions.PatronymicVisible = this.chkPatronymic.Checked;
			this.fOptions.ChartOptions.DiffLines = this.chkDiffLines.Checked;
			this.fOptions.ChartOptions.BirthDateVisible = this.chkBirthDate.Checked;
			this.fOptions.ChartOptions.DeathDateVisible = this.chkDeathDate.Checked;
			this.fOptions.ChartOptions.OnlyYears = this.chkOnlyYears.Checked;
			this.fOptions.ChartOptions.Kinship = this.chkKinship.Checked;
			this.fOptions.ChartOptions.SignsVisible = this.chkSignsVisible.Checked;
			this.fOptions.ChartOptions.ChildlessExclude = this.chkChildlessExclude.Checked;
			this.fOptions.ChartOptions.Decorative = this.chkTreeDecorative.Checked;
			this.fOptions.ChartOptions.PortraitsVisible = this.chkPortraitsVisible.Checked;
			this.fOptions.ChartOptions.MaleColor = this.PanMaleColor.BackColor;
			this.fOptions.ChartOptions.FemaleColor = this.PanFemaleColor.BackColor;
			this.fOptions.ChartOptions.UnkSexColor = this.PanUnkSexColor.BackColor;
			this.fOptions.ChartOptions.UnHusbandColor = this.PanUnHusbandColor.BackColor;
			this.fOptions.ChartOptions.UnWifeColor = this.PanUnWifeColor.BackColor;
			this.fOptions.Proxy.UseProxy = this.chkProxy.Checked;
			this.fOptions.Proxy.Server = this.edProxyServer.Text;
			this.fOptions.Proxy.Port = this.edProxyPort.Text;
			this.fOptions.Proxy.Login = this.edProxyLogin.Text;
			this.fOptions.Proxy.Password = this.edProxyPass.Text;
			this.fOptions.PedigreeOptions.IncludeAttributes = this.chkAttributes.Checked;
			this.fOptions.PedigreeOptions.IncludeNotes = this.chkNotes.Checked;
			this.fOptions.PedigreeOptions.IncludeSources = this.chkSources.Checked;

			if (this.RButton10.Checked)
			{
				this.fOptions.PedigreeOptions.Format = PedigreeOptions.PedigreeFormat.pfExcess;
			}
			else if (this.RButton11.Checked)
			{
				this.fOptions.PedigreeOptions.Format = PedigreeOptions.PedigreeFormat.pfCompact;
			}

			this.fOptions.ShowTips = this.chkShowOnStart.Checked;
			this.fOptions.RevisionsBackup = this.chkRevisionsBackup.Checked;

		    GKComboItem item = this.cbLanguages.Items[this.cbLanguages.SelectedIndex] as GKComboItem;
            if (item != null) {
                TfmGEDKeeper.Instance.LoadLanguage((int)item.Data);
            }

			base.DialogResult = DialogResult.OK;
		}

		private void btnColumnUp_Click(object sender, EventArgs e)
		{
			int idx = this.ListPersonColumns.SelectedIndex;
			if (idx > 0)
			{
				TColumnProps props = this.fTempColumns[idx - 1];
				this.fTempColumns[idx - 1] = this.fTempColumns[idx];
				this.fTempColumns[idx] = props;
				this.UpdateColumnsList();
				this.ListPersonColumns.SelectedIndex = idx - 1;
			}
		}

		private void btnColumnDown_Click(object sender, EventArgs e)
		{
			int idx = this.ListPersonColumns.SelectedIndex;
			if (idx >= 0 && idx < 24)
			{
				TColumnProps props = this.fTempColumns[idx + 1];
				this.fTempColumns[idx + 1] = this.fTempColumns[idx];
				this.fTempColumns[idx] = props;
				this.UpdateColumnsList();
				this.ListPersonColumns.SelectedIndex = idx + 1;
			}
		}

		private void btnDefList_Click(object sender, EventArgs e)
		{
			this.fTempColumns.ResetDefaults();
			this.UpdateColumnsList();
		}

		private void ListPersonColumns_ItemCheck(object sender, ItemCheckEventArgs e)
		{
			bool cs = (e.NewValue == CheckState.Checked);
			TColumnProps props = this.fTempColumns[e.Index];
			props.colActive = cs;
			this.fTempColumns[e.Index] = props;
		}

		void acbMouseClick(object sender, MouseEventArgs e)
		{
			Label lbl = sender as Label;
			this.ColorDialog1.Color = lbl.BackColor;
			if (this.ColorDialog1.ShowDialog() == DialogResult.OK) lbl.BackColor = this.ColorDialog1.Color;
		}

		public void SetPage(OptionsPage page)
		{
			switch (page) {
				case OptionsPage.opCommon:
					this.PageControl1.SelectTab(0);
					break;
				case OptionsPage.opTreeChart:
					this.PageControl1.SelectTab(1);
					this.tabControl1.SelectTab(0);
					break;
				case OptionsPage.opAncestorsCircle:
					this.PageControl1.SelectTab(1);
					this.tabControl1.SelectTab(1);
					break;
				case OptionsPage.opInterface:
					this.PageControl1.SelectTab(2);
					break;
				case OptionsPage.opPedigree:
					this.PageControl1.SelectTab(3);
					break;
			}
		}

		void ILocalization.SetLang()
		{
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_MIOptions);
			this.SheetCommon.Text = LangMan.LS(LSID.LSID_Common);
			this.SheetView.Text = LangMan.LS(LSID.LSID_Interface);
			this.SheetTree.Text = LangMan.LS(LSID.LSID_Trees);
			this.SheetPedigree.Text = LangMan.LS(LSID.LSID_Pedigrees);
			this.rgCode.Text = LangMan.LS(LSID.LSID_SaveCoding);

			this.GroupBox4.Text = LangMan.LS(LSID.LSID_Internet);
			this.chkProxy.Text = LangMan.LS(LSID.LSID_ProxyUse);
			this.Label1.Text = LangMan.LS(LSID.LSID_ProxyServer);
			this.Label2.Text = LangMan.LS(LSID.LSID_ProxyPort);
			this.Label3.Text = LangMan.LS(LSID.LSID_ProxyLogin);
			this.Label4.Text = LangMan.LS(LSID.LSID_ProxyPassword);

			this.GroupBox7.Text = LangMan.LS(LSID.LSID_Other);

			this.chkShowOnStart.Text = LangMan.LS(LSID.LSID_StartupTips);
			this.Label6.Text = LangMan.LS(LSID.LSID_Language);
			this.SheetViewCommon.Text = LangMan.LS(LSID.LSID_ListsAll);
			this.SheetViewPersons.Text = LangMan.LS(LSID.LSID_ListPersons);
			this.rgFNPFormat.Text = LangMan.LS(LSID.LSID_NamesFormat);
			this.RButton5.Text = LangMan.LS(LSID.LSID_NF1);
			this.RButton6.Text = LangMan.LS(LSID.LSID_NF2);
			this.RButton7.Text = LangMan.LS(LSID.LSID_NF3);
			this.rgDateFormat.Text = LangMan.LS(LSID.LSID_DateFormat);
			this.chkPlacesWithAddress.Text = LangMan.LS(LSID.LSID_PlacesWithAddress);
			this.chkHighlightUnparented.Text = LangMan.LS(LSID.LSID_HighlightUnparented);
			this.chkHighlightUnmarried.Text = LangMan.LS(LSID.LSID_HighlightUnmarried);
			this.btnDefList.Text = LangMan.LS(LSID.LSID_DefList);
			this.GroupBox1.Text = LangMan.LS(LSID.LSID_ViewTree);
			this.chkFamily.Text = LangMan.LS(LSID.LSID_Surname);
			this.chkName.Text = LangMan.LS(LSID.LSID_Name);
			this.chkPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
			this.chkDiffLines.Text = LangMan.LS(LSID.LSID_DiffLines);
			this.chkBirthDate.Text = LangMan.LS(LSID.LSID_BirthDate);
			this.chkDeathDate.Text = LangMan.LS(LSID.LSID_DeathDate);
			this.chkOnlyYears.Text = LangMan.LS(LSID.LSID_OnlyYears);
			this.chkKinship.Text = LangMan.LS(LSID.LSID_Kinship);
			this.chkSignsVisible.Text = LangMan.LS(LSID.LSID_SignsVisible);
			this.chkTreeDecorative.Text = LangMan.LS(LSID.LSID_TreeDecorative);
			this.chkPortraitsVisible.Text = LangMan.LS(LSID.LSID_PortraitsVisible);
			this.chkChildlessExclude.Text = LangMan.LS(LSID.LSID_ChildlessExclude);
			this.GroupBox2.Text = LangMan.LS(LSID.LSID_Decor);
			this.PanMaleColor.Text = LangMan.LS(LSID.LSID_Man);
			this.PanFemaleColor.Text = LangMan.LS(LSID.LSID_Woman);
			this.PanUnkSexColor.Text = LangMan.LS(LSID.LSID_UnkSex);
			this.PanUnHusbandColor.Text = LangMan.LS(LSID.LSID_UnHusband);
			this.PanUnWifeColor.Text = LangMan.LS(LSID.LSID_UnWife);
			this.Label5.Text = LangMan.LS(LSID.LSID_Font);
			this.GroupBox5.Text = LangMan.LS(LSID.LSID_PedigreeGen);
			this.chkAttributes.Text = LangMan.LS(LSID.LSID_IncludeAttributes);
			this.chkNotes.Text = LangMan.LS(LSID.LSID_IncludeNotes);
			this.chkSources.Text = LangMan.LS(LSID.LSID_IncludeSources);
			this.EditPedigreeFormat.Text = LangMan.LS(LSID.LSID_PedigreeFormat);
			this.RButton10.Text = LangMan.LS(LSID.LSID_PF1);
			this.RButton11.Text = LangMan.LS(LSID.LSID_PF2);
			this.chkRevisionsBackup.Text = LangMan.LS(LSID.LSID_RevisionsBackup);
			this.SheetCharts.Text = LangMan.LS(LSID.LSID_Charts);
			this.SheetPlugins.Text = LangMan.LS(LSID.LSID_Plugins);
		}
	}
}
