using System;
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Dialogs;

/// <summary>
/// 
/// </summary>

namespace GKUI
{
    public sealed partial class TfmGEDKeeper : Form, ILocalization, IHost
	{
		private NamesTable fNamesTable;
		private GlobalOptions fOptions;
        private List<IPlugin> fPlugins;

        private readonly List<IWidget> fActiveWidgets;
        private readonly string[] fCommandArgs;

		private static TfmGEDKeeper fInstance = null;

		public static TfmGEDKeeper Instance
		{
			get { return fInstance; }
		}

		public NamesTable NamesTable
		{
			get { return this.fNamesTable; }
		}

		public GlobalOptions Options
		{
			get { return this.fOptions; }
		}

		public List<IPlugin> Plugins
		{
			get { return this.fPlugins; }
		}
		
		#region Instance control

		public TfmGEDKeeper(string[] args)
		{
			this.InitializeComponent();

            fInstance = this;

            this.fCommandArgs = (string[])args.Clone();
            this.fActiveWidgets = new List<IWidget>();
            
            //LangMan.SaveDefaultLanguage();
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
                fNamesTable.Dispose();
                fOptions.Dispose();

                if (components != null) components.Dispose();
            }
			base.Dispose(Disposing);
		}

		#endregion

		#region Event handlers
		
		private void Form_Load(object sender, EventArgs e)
		{
			SysUtils.LogInit(this.GetAppDataPath() + "GEDKeeper2.log");

			this.fOptions = new GlobalOptions();
			this.fOptions.LoadFromFile(this.GetAppDataPath() + "GEDKeeper2.ini");
			this.fOptions.FindLanguages();

			if (this.fOptions.MWinRect.Left != -1 && this.fOptions.MWinRect.Top != -1 && this.fOptions.MWinRect.Right != -1 && this.fOptions.MWinRect.Bottom != -1)
			{
				base.Left = this.fOptions.MWinRect.Left;
				base.Top = this.fOptions.MWinRect.Top;
				base.Width = this.fOptions.MWinRect.Right;
				base.Height = this.fOptions.MWinRect.Bottom;
			}
			else
			{
				base.Left = (Screen.PrimaryScreen.WorkingArea.Width - 800) / 2;
				base.Top = (Screen.PrimaryScreen.WorkingArea.Height - 600) / 2;
				base.Width = 800;
				base.Height = 600;
			}
			base.WindowState = this.fOptions.MWinState;

			this.fNamesTable = new NamesTable();
			this.fNamesTable.LoadFromFile(this.GetAppDataPath() + "GEDKeeper2.nms");

			this.LoadPlugins(GKUtils.GetAppPath() + "\\plugins\\");
            this.UpdatePluginsItems();

			this.LoadLanguage(this.fOptions.InterfaceLang);

			this.UpdateMRU();
			this.UpdateControls(false);

			if (fCommandArgs.Length > 0) {
				this.CreateBase(fCommandArgs[0]);
			}
		}

		private void Form_Closed(object sender, FormClosedEventArgs e)
		{
			this.UnloadPlugins();

			this.fOptions.MWinRect = SysUtils.GetFormRect(this);
			this.fOptions.MWinState = base.WindowState;

            Win32Native.HtmlHelp(IntPtr.Zero, null, 18u, 0u);

			this.fNamesTable.SaveToFile(this.GetAppDataPath() + "GEDKeeper2.nms");
			this.fNamesTable.Dispose();

			this.fOptions.SaveToFile(this.GetAppDataPath() + "GEDKeeper2.ini");
			this.fOptions.Dispose();
		}

		private void Form_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.F12)
			{
				// dummy
			}
		}

		private void Form_Show(object sender, EventArgs e)
		{
			int num = this.fOptions.GetLastBasesCount();
			for (int i = 0; i < num; i++) {
				string lb = this.fOptions.GetLastBase(i);
				if (File.Exists(lb)) {
                    this.CreateBase(lb);
				}
			}
		}

		private void Form_Resize(object sender, EventArgs e)
		{
			this.StatusBar.Panels[0].Width = base.Width - 50;
		}

		private void Form_Closing(object sender, FormClosingEventArgs e)
		{
			this.fOptions.ClearLastBases();
			for (int i = base.MdiChildren.Length - 1; i >= 0; i--)
			{
				Form mdiChild = base.MdiChildren[i];
				if (mdiChild is TfmBase)
				{
					this.fOptions.AddLastBase((mdiChild as TfmBase).Tree.FileName);
				}
			}
		}

		private void Form_DragEnter(object sender, DragEventArgs e)
		{
		    e.Effect = e.Data.GetDataPresent(DataFormats.FileDrop) ? DragDropEffects.Copy : DragDropEffects.None;
		}

	    private void Form_DragDrop(object sender, DragEventArgs e)
		{
			try
			{
				Array a = e.Data.GetData(DataFormats.FileDrop) as Array;
				if (a != null)
				{
					for (int i = 0; i < a.Length; i++) {
						string fn = a.GetValue(i).ToString();
						this.CreateBase(fn);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TfmGEDKeeper.Form_DragDrop(): " + ex.Message);
			}
		}

		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);

			if (m.Msg == Win32Native.WM_KEEPMODELESS)
			{
				foreach (IWidget widget in this.fActiveWidgets)
				{
					widget.WidgetEnable();
				}
			}
		}

		private void StatusBar_DrawItem(object sender, StatusBarDrawItemEventArgs sbdevent)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    Bitmap pic = null;
		    switch (curBase.ShieldState) {
		        case ShieldState.ssNone:
		            pic = (Bitmap)GKResources.iRGShieldNone.Clone();
		            break;
		        case ShieldState.ssMiddle:
		            pic = (Bitmap)GKResources.iRGShieldMid.Clone();
		            break;
		        case ShieldState.ssMaximum:
		            pic = (Bitmap)GKResources.iRGShieldMax.Clone();
		            break;
		    }

		    if (pic != null) {
		        pic.MakeTransparent(pic.GetPixel(0, 0));
		        sbdevent.Graphics.DrawImage(pic, sbdevent.Bounds.Left, sbdevent.Bounds.Top);
		    }
		}

		private void StatusBar_PanelClick(object sender, StatusBarPanelClickEventArgs e)
		{
			if (e.StatusBarPanel == StatusBarPanel2 && e.Clicks == 2) {
				IBase curBase = this.GetCurrentFile();
				if (curBase == null) return;

                ShieldState ss = curBase.ShieldState;
				if (ss == ShieldState.ssNone) {
					ss = ShieldState.ssMaximum;
				} else {
					ss = (ShieldState)((int)ss + 1);
				}

                curBase.ShieldState = ss;
				StatusBar.Invalidate();
			}
		}

		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (e.Button == this.tbFileNew) {
				this.miFileNewClick(null, null);
			} else if (e.Button == this.tbFileLoad) {
				this.miFileLoadClick(null, null);
			} else if (e.Button == this.tbFileSave) {
				this.miFileSaveClick(null, null);
			} else if (e.Button == this.tbRecordAdd) {
				this.miRecordAddClick(null, null);
			} else if (e.Button == this.tbRecordEdit) {
				this.miRecordEditClick(null, null);
			} else if (e.Button == this.tbRecordDelete) {
				this.miRecordDeleteClick(null, null);
			} else if (e.Button == this.tbFilter) {
				this.miFilterClick(null, null);
			} else if (e.Button == this.tbTreeAncestors) {
				this.miTreeAncestorsClick(null, null);
			} else if (e.Button == this.tbTreeDescendants) {
				this.miTreeDescendantsClick(null, null);
			} else if (e.Button == this.tbTreeBoth) {
				this.miTreeBothClick(null, null);
			} else if (e.Button == this.tbStats) {
				this.miStatsClick(null, null);
			} else if (e.Button == this.tbPrev) {
				this.tbPrevClick(null, null);
			} else if (e.Button == this.tbNext) {
				this.tbNextClick(null, null);
			}
		}

		private void miExit_Click(object sender, EventArgs e)
		{
			base.Close();
		}

	    #endregion

	    #region Misc functions
	    
		public void LoadLanguage(int langCode)
		{
			if (langCode != LangMan.LSDefCode)
			{
				bool loaded = false;

				int num = this.fOptions.GetLangsCount();
				for (int i = 0; i < num; i++) {
					if (this.fOptions.GetLang(i).Code == langCode) {
						loaded = LangMan.LoadFromFile(this.fOptions.GetLang(i).FileName);
						break;
					}
				}

				if (!loaded) langCode = LangMan.LSDefCode;
			}

			if (langCode == LangMan.LSDefCode) {
				LangMan.DefInit();
			}

			GKData.DataSetup();

			int num2 = base.MdiChildren.Length;
			for (int i = 0; i < num2; i++) {
				Form child = base.MdiChildren[i];
				if (child is ILocalization) (child as ILocalization).SetLang();
			}

			(this as ILocalization).SetLang();

			this.fOptions.InterfaceLang = (ushort)langCode;
			
			this.UpdatePluginsLanguage();
		}

		public DialogResult ShowModalEx(Form form, bool keepModeless)
		{
            if (form == null) return DialogResult.None;

			if (keepModeless)
			{
                Win32Native.PostMessage(this.Handle, Win32Native.WM_KEEPMODELESS, 0, 0);
			}

			return form.ShowDialog();
		}

		#endregion
		
		#region MRU functions
		
		private void MRUFileClick(object sender, EventArgs e)
		{
			int idx = ((GKMenuItem)sender).Tag;
			this.CreateBase(this.fOptions.MRUFiles[idx].FileName);
		}

		private void UpdateMRU()
		{
			this.miMRUFiles.Enabled = (this.fOptions.MRUFiles.Count > 0);
			this.miMRUFiles.MenuItems.Clear();
			this.MenuMRU.MenuItems.Clear();

			int num = this.fOptions.MRUFiles.Count;
			for (int i = 0; i < num; i++) {
				string fn = this.fOptions.MRUFiles[i].FileName;

				MenuItem mi = new GKMenuItem(fn, i);
				mi.Click += this.MRUFileClick;
				this.miMRUFiles.MenuItems.Add(mi);

				mi = new GKMenuItem(fn, i);
				mi.Click += this.MRUFileClick;
				this.MenuMRU.MenuItems.Add(mi);
			}
		}

		public void AddMRU(string fileName)
		{
			int idx = this.fOptions.MRUFiles_IndexOf(fileName);
			if (idx >= 0)
			{
				GlobalOptions.TMRUFile tmp_mf = this.fOptions.MRUFiles[0];
				this.fOptions.MRUFiles[0] = this.fOptions.MRUFiles[idx];
				this.fOptions.MRUFiles[idx] = tmp_mf;
			}
			else
			{
				GlobalOptions.TMRUFile new_mf = new GlobalOptions.TMRUFile();
				new_mf.FileName = fileName;
				this.fOptions.MRUFiles.Insert(0, new_mf);
			}

			this.UpdateMRU();
		}

		public void CheckMRUWin(string fileName, Form frm)
		{
			int idx = this.fOptions.MRUFiles_IndexOf(fileName);
			if (idx >= 0)
			{
				GlobalOptions.TMRUFile mf = this.fOptions.MRUFiles[idx];
				
				mf.WinRect = SysUtils.GetFormRect(frm);
				mf.WinState = frm.WindowState;
			}
		}

		public void RestoreMRU(IBase aBase, string fileName)
		{
			int idx = this.fOptions.MRUFiles_IndexOf(fileName);
			if (idx >= 0)
			{
				GlobalOptions.TMRUFile mf = this.fOptions.MRUFiles[idx];
				SysUtils.SetFormRect(aBase as Form, mf.WinRect, mf.WinState);
			}
		}
		
		#endregion

		#region Base Management
	    
		public void UpdateControls(bool forceDeactivate)
		{
			try
			{
				IBase curBase = ((forceDeactivate) ? null : this.GetCurrentFile());
				TfmChart curChart = ((this.ActiveMdiChild is TfmChart) ? (this.ActiveMdiChild as TfmChart) : null);

				IWorkWindow workWin = ((curChart != null) ? (curChart as IWorkWindow) : curBase);
				
				TGEDCOMRecordType rt = (curBase == null) ? TGEDCOMRecordType.rtNone : curBase.GetSelectedRecordType();
				bool base_en = (rt != TGEDCOMRecordType.rtNone);

				this.miFileSave.Enabled = base_en || (curChart != null);
				this.tbFileSave.Enabled = this.miFileSave.Enabled;
				this.miRecordAdd.Enabled = base_en;
				this.tbRecordAdd.Enabled = this.miRecordAdd.Enabled;
				this.miRecordEdit.Enabled = base_en;
				this.tbRecordEdit.Enabled = this.miRecordEdit.Enabled;
				this.miRecordDelete.Enabled = base_en;
				this.tbRecordDelete.Enabled = this.miRecordDelete.Enabled;
				this.miStats.Enabled = base_en;
				this.tbStats.Enabled = this.miStats.Enabled;
				this.miFilter.Enabled = base_en;
				this.tbFilter.Enabled = this.miFilter.Enabled;

				this.miTreeTools.Enabled = base_en;
				this.miExportToFamilyBook.Enabled = base_en;
				this.miExportToExcelFile.Enabled = base_en;
				this.miFileClose.Enabled = base_en;
				this.miFileProperties.Enabled = base_en;
				this.miOrganizer.Enabled = base_en;
				this.miScripts.Enabled = base_en;
				
				bool indiv_en = base_en && rt == TGEDCOMRecordType.rtIndividual;
				
				this.miTreeAncestors.Enabled = indiv_en;
				this.tbTreeAncestors.Enabled = this.miTreeAncestors.Enabled;
				this.miTreeDescendants.Enabled = indiv_en;
				this.tbTreeDescendants.Enabled = this.miTreeDescendants.Enabled;
				this.miTreeBoth.Enabled = indiv_en;
				this.tbTreeBoth.Enabled = this.miTreeBoth.Enabled;
				this.miPedigree.Enabled = indiv_en;
				this.tbPedigree.Enabled = this.miPedigree.Enabled;
				this.miPedigree_dAboville.Enabled = indiv_en;
				this.miPedigree_Konovalov.Enabled = indiv_en;

				this.tbPrev.Enabled = (curBase != null && curBase.NavCanBackward());
				this.tbNext.Enabled = (curBase != null && curBase.NavCanForward());

				if (workWin != null) {
					this.StatusBar.Panels[0].Text = workWin.GetStatusString();
				}

				this.StatusBar.Invalidate();
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TfmGEDKeeper.UpdateControls(): " + ex.Message);
			}
		}

		public string GetCurrentFileName()
		{
			IBase cb = this.GetCurrentFile();
			string result = ((cb == null) ? "" : cb.Tree.FileName);
			return result;
		}

		public IBase CreateBase(string fileName)
		{
			IBase result = new TfmBase();
			result.Show();

			if (fileName != "" && File.Exists(fileName)) {
				result.FileLoad(fileName);
			} else {
				result.FileNew();
			}

			this.RestoreMRU(result, fileName);

			return result;
		}

		public void CriticalSave()
		{
			try
			{
				int num = base.MdiChildren.Length;
				for (int i = 0; i < num; i++)
				{
					Form child = base.MdiChildren[i];

					if (child is TfmBase) {
						TGEDCOMTree tree = (child as TfmBase).Tree;
						
						string rfn = Path.ChangeExtension(tree.FileName, ".restore");
						tree.SaveToFile(rfn, this.fOptions.DefCharacterSet);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TfmGEDKeeper.CriticalSave(): " + ex.Message);
			}
		}

		private void miExportToFamilyBookClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			using (FamilyBookExporter fb = new FamilyBookExporter(curBase))
			{
				fb.Generate(true);
			}
		}

		private void miExportToExcelFileClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			using (ExcelExporter exExp = new ExcelExporter(curBase))
			{
				exExp.Options = this.fOptions;
				exExp.SelectedRecords = curBase.GetContentList(TGEDCOMRecordType.rtIndividual);
				exExp.AppMode = false;
				exExp.Generate(true);
			}
		}

		private void miFilePropertiesClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			using (TfmFileProperties dlgFileProps = new TfmFileProperties(curBase))
			{
				TfmGEDKeeper.Instance.ShowModalEx(dlgFileProps, false);
			}
		}

		private void miScriptsClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			using (TfmScriptDaemon dmn = new TfmScriptDaemon(curBase))
			{
				TfmGEDKeeper.Instance.ShowModalEx(dmn, false);
			}
		}

		private void miTreeToolsClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			using (TfmTreeTools fmTreeTools = new TfmTreeTools(curBase))
			{
				TfmGEDKeeper.Instance.ShowModalEx(fmTreeTools, false);
			}
		}

		private void miOptionsClick(object sender, EventArgs e)
		{
			TfmOptions fmOptions = new TfmOptions(this);
			try
			{
				Form activeForm = this.ActiveMdiChild;
				if (activeForm is TfmBase) fmOptions.SetPage(OptionsPage.opInterface);
				if (activeForm is TfmChart) fmOptions.SetPage(OptionsPage.opTreeChart);

				if (fmOptions.ShowDialog() == DialogResult.OK)
				{
					int num = base.MdiChildren.Length;
					for (int i = 0; i < num; i++)
					{
						Form child = base.MdiChildren[i];

						if (child is TfmBase) {
							(child as TfmBase).RefreshLists(true);
						} else if (child is TfmChart) {
							(child as TfmChart).GenChart(false);
						}
					}
				}
			}
			finally
			{
				fmOptions.Dispose();
			}
		}

		private void miFileCloseClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    curBase.Close();
		}

		private void miMapClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    TfmMaps frm_maps = new TfmMaps(curBase);
			frm_maps.MdiParent = this;
			frm_maps.Show();
		}

		private void miOrganizerClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			using (TfmOrganizer dlg = new TfmOrganizer(curBase))
			{
				TfmGEDKeeper.Instance.ShowModalEx(dlg, false);
			}
		}

		private void miFilterClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    curBase.SetFilter();
		}

		private void tbPrevClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    curBase.NavPrev();
		}

		private void tbNextClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    curBase.NavNext();
		}

		private void miStatsClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			TfmStats fmStats = new TfmStats(curBase);
			fmStats.Show();
		}

		private void miFileNewClick(object sender, EventArgs e)
		{
			this.CreateBase("");
		}

		private void miFileLoadClick(object sender, EventArgs e)
		{
			this.OpenDialog1.InitialDirectory = this.fOptions.LastDir;
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				this.CreateBase(this.OpenDialog1.FileName);
			}
		}

		public void miFileSaveClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile(true);
		    if (curBase == null) return;

		    this.SaveDialog1.FileName = curBase.Tree.FileName;
		    if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
		    {
		        curBase.FileSave(this.SaveDialog1.FileName);
		    }
		}

		private void miTreeAncestorsClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			if (TfmChart.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TTreeChartBox.TChartKind.ckAncestors))
			{
				TfmChart fmChart = new TfmChart(curBase, curBase.GetSelectedPerson());
				fmChart.ChartKind = TTreeChartBox.TChartKind.ckAncestors;
				fmChart.GenChart(true);
			}
		}

		private void miTreeDescendantsClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			if (TfmChart.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TTreeChartBox.TChartKind.ckDescendants))
			{
				TfmChart fmChart = new TfmChart(curBase, curBase.GetSelectedPerson());
				fmChart.ChartKind = TTreeChartBox.TChartKind.ckDescendants;
				fmChart.GenChart(true);
			}
		}

		private void miPedigree_dAbovilleClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			using (PedigreeExporter p = new PedigreeExporter(curBase))
			{
				p.Ancestor = curBase.GetSelectedPerson();
				p.Options = this.fOptions;
				p.ShieldState = curBase.ShieldState;
				p.Kind = PedigreeExporter.TPedigreeKind.pk_dAboville;
				p.Generate(true);
			}
		}

		private void miPedigree_KonovalovClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			using (PedigreeExporter p = new PedigreeExporter(curBase))
			{
				p.Ancestor = curBase.GetSelectedPerson();
				p.Options = this.fOptions;
				p.ShieldState = curBase.ShieldState;
				p.Kind = PedigreeExporter.TPedigreeKind.pk_Konovalov;
				p.Generate(true);
			}
		}

		private void miRecordAddClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    curBase.RecordAdd();
		}

		private void miRecordEditClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    curBase.RecordEdit(sender, e);
		}

		private void miRecordDeleteClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

		    curBase.RecordDelete();
		}

		private void miTreeBothClick(object sender, EventArgs e)
		{
			IBase curBase = this.GetCurrentFile();
		    if (curBase == null) return;

			if (TfmChart.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TTreeChartBox.TChartKind.ckBoth))
			{
				TfmChart fmChart = new TfmChart(curBase, curBase.GetSelectedPerson());
				fmChart.ChartKind = TTreeChartBox.TChartKind.ckBoth;
				fmChart.GenChart(true);
			}
		}

		#endregion
		
		#region Help and Windows
		
		private void miLogSendClick(object sender, EventArgs e)
		{
			SysUtils.LogSend();
		}

		private void miLogViewClick(object sender, EventArgs e)
		{
			SysUtils.LogView();
		}

		private void miAboutClick(object sender, EventArgs e)
		{
			TfmAbout.ShowAbout("GEDKeeper");
		}

		public void ShowHelpTopic(string topic)
		{
			string fns = GKUtils.GetAppPath() + "GEDKeeper2.chm" + topic;
            Win32Native.HtmlHelp(this.Handle, fns, 0u, 0u);
		}

		private void miGenResourcesClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhGenRes.htm");
		}

		private void miKinshipTermsClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhRelations.htm");
		}

		private void miFAQClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhFAQ.htm");
		}

		private void miContextClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("");
		}

		private void miWinCascadeClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.Cascade);
		}

		private void miWinHTileClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.TileHorizontal);
		}

		private void miWinVTileClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.TileVertical);
		}

		private void miWinMinimizeClick(object sender, EventArgs e)
		{
			Form[] mdiChildren = base.MdiChildren;
			for (int I = mdiChildren.Length - 1; I >= 0; I--)
			{
				mdiChildren[I].WindowState = FormWindowState.Minimized;
			}
		}

		private void miWinArrangeClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.ArrangeIcons);
		}

		#endregion

		#region ILocalization implementation

		void ILocalization.SetLang()
		{
			this.miFile.Text = LangMan.LS(LSID.LSID_MIFile);
			this.miEdit.Text = LangMan.LS(LSID.LSID_MIEdit);
			this.miPedigree.Text = LangMan.LS(LSID.LSID_MIPedigree);
			this.miService.Text = LangMan.LS(LSID.LSID_MIService);
			this.miWindow.Text = LangMan.LS(LSID.LSID_MIWindow);
			this.miHelp.Text = LangMan.LS(LSID.LSID_MIHelp);

			this.miFileNew.Text = LangMan.LS(LSID.LSID_MIFileNew);
			this.miFileLoad.Text = LangMan.LS(LSID.LSID_MIFileLoad);
			this.miMRUFiles.Text = LangMan.LS(LSID.LSID_MIMRUFiles);
			this.miFileSave.Text = LangMan.LS(LSID.LSID_MIFileSave);
			this.miFileClose.Text = LangMan.LS(LSID.LSID_MIFileClose);
			this.miFileProperties.Text = LangMan.LS(LSID.LSID_MIFileProperties);
			this.miExport.Text = LangMan.LS(LSID.LSID_MIExport);
			this.miExportToFamilyBook.Text = LangMan.LS(LSID.LSID_MIExportToFamilyBook);
			this.miExportToExcelFile.Text = LangMan.LS(LSID.LSID_MIExportToExcelFile);
			this.miExit.Text = LangMan.LS(LSID.LSID_MIExit);

			this.miRecordAdd.Text = LangMan.LS(LSID.LSID_MIRecordAdd);
			this.miRecordEdit.Text = LangMan.LS(LSID.LSID_MIRecordEdit);
			this.miRecordDelete.Text = LangMan.LS(LSID.LSID_MIRecordDelete);

			this.miTreeAncestors.Text = LangMan.LS(LSID.LSID_MITreeAncestors);
			this.miTreeDescendants.Text = LangMan.LS(LSID.LSID_MITreeDescendants);
			this.miTreeBoth.Text = LangMan.LS(LSID.LSID_MITreeBoth);
			this.miPedigree_dAboville.Text = LangMan.LS(LSID.LSID_MIPedigree_dAboville);
			this.miPedigree_Konovalov.Text = LangMan.LS(LSID.LSID_MIPedigree_Konovalov);
			this.miMap.Text = LangMan.LS(LSID.LSID_MIMap) + "...";
			this.miStats.Text = LangMan.LS(LSID.LSID_MIStats) + "...";

			this.miOrganizer.Text = LangMan.LS(LSID.LSID_MIOrganizer) + "...";
			this.miScripts.Text = LangMan.LS(LSID.LSID_MIScripts);
			this.miTreeTools.Text = LangMan.LS(LSID.LSID_MITreeTools);
			this.miFilter.Text = LangMan.LS(LSID.LSID_MIFilter) + "...";
			this.miOptions.Text = LangMan.LS(LSID.LSID_MIOptions) + "...";

			this.miWinCascade.Text = LangMan.LS(LSID.LSID_MIWinCascade);
			this.miWinHTile.Text = LangMan.LS(LSID.LSID_MIWinHTile);
			this.miWinVTile.Text = LangMan.LS(LSID.LSID_MIWinVTile);
			this.miWinMinimize.Text = LangMan.LS(LSID.LSID_MIWinMinimize);
			this.miWinArrange.Text = LangMan.LS(LSID.LSID_MIWinArrange);

			this.miGenResources.Text = LangMan.LS(LSID.LSID_MIGenResources);
			this.miKinshipTerms.Text = LangMan.LS(LSID.LSID_MIKinshipTerms);
			this.miFAQ.Text = LangMan.LS(LSID.LSID_MIFAQ);
			this.miContext.Text = LangMan.LS(LSID.LSID_MIContext);
			this.miAbout.Text = LangMan.LS(LSID.LSID_MIAbout) + "...";

			this.miLogSend.Text = LangMan.LS(LSID.LSID_LogSend);
			this.miPlugins.Text = LangMan.LS(LSID.LSID_Plugins);
		}

		#endregion

		#region Plugins support

		public PluginInfo GetPluginAttributes(IPlugin plugin)
		{
			PluginInfo info = new PluginInfo();

			Assembly asm = plugin.GetType().Assembly;
			
			var attr1 = asm.GetAssemblyAttribute<AssemblyTitleAttribute>();
			if (attr1 != null) info.Title = attr1.Title;
			
			var attr2 = asm.GetAssemblyAttribute<AssemblyDescriptionAttribute>();
			if (attr2 != null) info.Description = attr2.Description;
			
			var attr3 = asm.GetAssemblyAttribute<AssemblyCopyrightAttribute>();
			if (attr3 != null) info.Copyright = attr3.Copyright;
			
			var attr4 = asm.GetAssemblyAttribute<AssemblyFileVersionAttribute>();
			if (attr4 != null) info.Version = attr4.Version;
			
			return info;
		}
		
		private void UpdatePluginsLanguage()
		{
			if (this.fPlugins == null) return;
			
			int num = this.fPlugins.Count;
			for (int i = 0; i < num; i++) {
				IPlugin plugin = this.fPlugins[i];
				plugin.OnLanguageChange();
			}

			num = this.miPlugins.MenuItems.Count;
			for (int i = 0; i < num; i++) {
				MenuItem mi = this.miPlugins.MenuItems[i];
				IPlugin plugin = mi.Tag as IPlugin;
				mi.Text = plugin.DisplayName;
			}
		}
		
        private void Plugin_Click(object sender, EventArgs e)
        {
        	MenuItem item = sender as MenuItem;
        	if (item == null) return;

        	IPlugin plugin = item.Tag as IPlugin;
        	if (plugin == null) return;

            plugin.Execute();
        }

        private void UpdatePluginsItems()
        {
			this.miPlugins.Visible = (this.fPlugins.Count > 0);
			this.miPlugins.MenuItems.Clear();

			int num = this.fPlugins.Count;
			for (int i = 0; i < num; i++)
			{
				string dispName = this.fPlugins[i].DisplayName;

				MenuItem mi = new GKMenuItem(dispName, i);
				mi.Click += this.Plugin_Click;
				mi.Tag = this.fPlugins[i];
				this.miPlugins.MenuItems.Add(mi);
				
				if (this.fPlugins[i] is IWidget) {
					(this.fPlugins[i] as IWidget).WidgetInit(this, mi);
				}
			}
        }

        private void UnloadPlugins()
        {
			if (this.fPlugins == null) return;
			
			int num = this.fPlugins.Count;
			for (int i = 0; i < num; i++)
			{
				IPlugin plugin = this.fPlugins[i];
				plugin.Shutdown();
			}
        }

        private void LoadPlugins(string path)
        {
        	AppDomain.CurrentDomain.AppendPrivatePath(path);
        	//AppDomain.CurrentDomain.SetupInformation.PrivateBinPath = path;
        	
            this.fPlugins = new List<IPlugin>();

            Type pluginType = typeof(IPlugin);
            string[] pluginFiles = Directory.GetFiles(path, "*.dll");

            for (int i = 0; i < pluginFiles.Length; i++)
            {
            	string pfn = pluginFiles[i];

                try
                {
                    Assembly asm;

                    try {
                    	asm = Assembly.LoadFile(pfn);
                    } catch {
                    	asm = null; // block exceptions for bad or non-dotnet assemblies
                    }

                    if (asm != null)
                    {
						Type[] types = asm.GetTypes();

						foreach (Type type in types)
						{
							if (type.IsInterface || type.IsAbstract) {
								continue;
							} else {
								if (type.GetInterface(pluginType.FullName) != null) {
									IPlugin plugin = (IPlugin)Activator.CreateInstance(type);
									plugin.Startup(this);
									
									this.fPlugins.Add(plugin);
								}
							}
						}
                    }
                }
                catch (Exception ex)
                {
                	SysUtils.LogWrite("TfmGEDKeeper.LoadPlugins(" +pfn+ "): " + ex.Message);
                }
            }
        }

        #endregion

        #region IHost implementation

        public ILangMan CreateLangMan(object sender)
        {
			CultureInfo cultInfo = new CultureInfo(this.fOptions.InterfaceLang);
			string ext = cultInfo.ThreeLetterISOLanguageName;

			Assembly asm = sender.GetType().Assembly;
			Module[] mods = asm.GetModules();
			string asmFile = mods[0].FullyQualifiedName;
			string langFile = Path.ChangeExtension(asmFile, "." + ext);

			LangManager langMan = new LangManager();
        	bool res = langMan.LoadFromFile(langFile);
        	return langMan;
        }

		public IBase GetCurrentFile(bool extMode = false)
		{
			TfmChart curChart = ((this.ActiveMdiChild is TfmChart) ? (this.ActiveMdiChild as TfmChart) : null);
			IBase result;

			if (extMode && curChart != null) {
				result = curChart.Base;
			} else {
				result = ((base.ActiveMdiChild is TfmBase) ? (base.ActiveMdiChild as TfmBase) : null);
			}

			return result;
		}

        public bool Register(IPlugin plugin)
        {
            return true;
        }

        public void LogWrite(string msg)
        {
        	SysUtils.LogWrite(msg);
        }

        public void NotifyRecord(IBase aBase, object record, RecordAction action)
        {
        	if (this.fPlugins == null) return;
        	if (aBase == null || record == null) return;
			
			int num = this.fPlugins.Count;
			for (int i = 0; i < num; i++)
			{
				IPlugin plugin = this.fPlugins[i];
				
				if (plugin is ISubscriber) {
					(plugin as ISubscriber).NotifyRecord(aBase, record, action);
				}
			}
        }

		public string GetAppDataPath()
		{
			string path = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + "\\" + GKData.AppTitle + "\\";
			if (!Directory.Exists(path)) Directory.CreateDirectory(path);
			return path;
		}

        public void WidgetShow(IWidget widget)
        {
            if (this.fActiveWidgets.Contains(widget)) return;
	    	this.fActiveWidgets.Add(widget);

	    	if (widget.MenuItem != null) widget.MenuItem.Checked = true;
        }

        public void WidgetClose(IWidget widget)
        {
            if (!this.fActiveWidgets.Contains(widget)) return;
            this.fActiveWidgets.Remove(widget);

            if (widget.MenuItem != null) widget.MenuItem.Checked = false;
        }

        public void BaseChanged(IBase aBase)
        {
            foreach (IWidget widget in this.fActiveWidgets)
            {
                widget.BaseChanged(aBase);
            }
        }
        
        public void ShowMDI(Form form)
        {
        	if (form != null) {
        		form.MdiParent = this;
        		form.Show();
        	}
        }

		#endregion
    }
}
