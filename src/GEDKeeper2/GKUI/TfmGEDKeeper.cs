using System;
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Security.Permissions;
using System.Windows.Forms;

using ExtUtils.MapiMail;
using ExtUtils.SingleInstancing;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Dialogs;

namespace GKUI
{
	/// <summary>
	/// 
	/// </summary>
    public sealed partial class TfmGEDKeeper : Form, IHost, ISingleInstanceEnforcer
	{
		private delegate void OnMessageReceivedInvoker(MessageEventArgs e);

		private class WidgetInfo
		{
			public IWidget Widget;
			public MenuItem MenuItem;
		}

		private NamesTable fNamesTable;
		private GlobalOptions fOptions;
		private List<IPlugin> fPlugins;

		private readonly List<WidgetInfo> fActiveWidgets;
		private string[] fCommandArgs;
		private string fLogFilename;

		private static TfmGEDKeeper fInstance = null;

		public static TfmGEDKeeper Instance
		{
			get { return fInstance; }
		}

		public INamesTable NamesTable
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

		public TfmGEDKeeper()
		{
			this.InitializeComponent();

			fInstance = this;

			this.fActiveWidgets = new List<WidgetInfo>();

			//LangMan.SaveDefaultLanguage();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				fNamesTable.Dispose();
				fOptions.Dispose();

				if (components != null) components.Dispose();
			}
			base.Dispose(disposing);
		}

		#endregion

		#region Event handlers

		public void SetArgs(string[] args)
		{
            if (args == null) return;

			this.fCommandArgs = (string[])args.Clone();
		}

		private void LoadArgs()
		{
			if (fCommandArgs != null && fCommandArgs.Length > 0) {
				this.CreateBase(fCommandArgs[0]);
			}
		}

		private void Form_Load(object sender, EventArgs e)
		{
			this.fLogFilename = this.GetAppDataPath() + "GEDKeeper2.log"; 
			Logger.LogInit(this.fLogFilename);

			this.fOptions = GlobalOptions.Instance;
			this.fOptions.LoadFromFile(this.GetAppDataPath() + "GEDKeeper2.ini");
			this.fOptions.FindLanguages();

			if (this.fOptions.MWinRect.Left != -1 && this.fOptions.MWinRect.Top != -1 && this.fOptions.MWinRect.Right != -1 && this.fOptions.MWinRect.Bottom != -1) {
				base.Left = this.fOptions.MWinRect.Left;
				base.Top = this.fOptions.MWinRect.Top;
				base.Width = this.fOptions.MWinRect.Right;
				base.Height = this.fOptions.MWinRect.Bottom;
			} else {
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

			this.LoadArgs();
		}

		private void Form_Closed(object sender, FormClosedEventArgs e)
		{
			this.UnloadPlugins();

			this.fOptions.MWinRect = GKUtils.GetFormRect(this);
			this.fOptions.MWinState = base.WindowState;

			SysUtils.HtmlHelp(IntPtr.Zero, null, 18u, 0u);

			this.fNamesTable.SaveToFile(this.GetAppDataPath() + "GEDKeeper2.nms");
			this.fNamesTable.Dispose();

			this.fOptions.SaveToFile(this.GetAppDataPath() + "GEDKeeper2.ini");
			this.fOptions.Dispose();
		}

		private void Form_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.F12) {
				// dummy
				IBaseWindow curBase = this.GetCurrentFile();
				if (curBase == null) return;

				using (TreesAlbumExporter p = new TreesAlbumExporter(curBase)) {
					p.Ancestor = curBase.GetSelectedPerson();
					p.Options = this.fOptions;
					p.ShieldState = curBase.ShieldState;
					p.Generate(true);
				}
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
			for (int i = base.MdiChildren.Length - 1; i >= 0; i--) {
				Form mdiChild = base.MdiChildren[i];
				if (mdiChild is IBaseWindow) {
					this.fOptions.AddLastBase((mdiChild as IBaseWindow).Tree.FileName);
				}
			}
		}

		private void Form_DragEnter(object sender, DragEventArgs e)
		{
			e.Effect = e.Data.GetDataPresent(DataFormats.FileDrop) ? DragDropEffects.Copy : DragDropEffects.None;
		}

		private void Form_DragDrop(object sender, DragEventArgs e)
		{
			try {
				Array a = e.Data.GetData(DataFormats.FileDrop) as Array;
				if (a != null) {
					for (int i = 0; i < a.Length; i++) {
						string fn = a.GetValue(i).ToString();
						this.CreateBase(fn);
					}
				}
			} catch (Exception ex) {
				this.LogWrite("TfmGEDKeeper.Form_DragDrop(): " + ex.Message);
			}
		}

        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);

			if (m.Msg == SysUtils.WM_KEEPMODELESS) {
				foreach (WidgetInfo widgetInfo in this.fActiveWidgets) {
					widgetInfo.Widget.WidgetEnable();
				}
			}
		}

		private void StatusBar_DrawItem(object sender, StatusBarDrawItemEventArgs sbdevent)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			Bitmap pic = null;
			switch (curBase.ShieldState)
			{
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
				IBaseWindow curBase = this.GetCurrentFile();
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
			} else if (e.Button == this.tbDocPrint) {
				this.tbDocPrintClick(null, null);
			} else if (e.Button == this.tbDocPreview) {
				this.tbDocPreviewClick(null, null);
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
			if (langCode != LangMan.LSDefCode) {
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

			if (keepModeless) {
				SysUtils.PostMessage(this.Handle, SysUtils.WM_KEEPMODELESS, IntPtr.Zero, IntPtr.Zero);
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
			if (idx >= 0) {
				MRUFile tmp_mf = this.fOptions.MRUFiles[0];
				this.fOptions.MRUFiles[0] = this.fOptions.MRUFiles[idx];
				this.fOptions.MRUFiles[idx] = tmp_mf;
			} else {
				MRUFile new_mf = new MRUFile();
				new_mf.FileName = fileName;
				this.fOptions.MRUFiles.Insert(0, new_mf);
			}

			this.UpdateMRU();
		}

		public void CheckMRUWin(string fileName, Form frm)
		{
			int idx = this.fOptions.MRUFiles_IndexOf(fileName);
			if (idx >= 0) {
				MRUFile mf = this.fOptions.MRUFiles[idx];

				mf.WinRect = GKUtils.GetFormRect(frm);
				mf.WinState = frm.WindowState;
			}
		}

		public void RestoreMRU(IBaseWindow aBase, string fileName)
		{
			int idx = this.fOptions.MRUFiles_IndexOf(fileName);
			if (idx >= 0) {
				MRUFile mf = this.fOptions.MRUFiles[idx];
				GKUtils.SetFormRect(aBase as Form, mf.WinRect, mf.WinState);
			}
		}

		#endregion

		#region Base Management

		public void UpdateNavControls()
		{
			try
			{
				IWorkWindow workWin = this.GetWorkWindow();

				this.tbPrev.Enabled = (workWin != null && workWin.NavCanBackward());
				this.tbNext.Enabled = (workWin != null && workWin.NavCanForward());
			} catch (Exception ex) {
				this.LogWrite("TfmGEDKeeper.UpdateNavControls(): " + ex.Message);
			}
		}

		public void UpdateControls(bool forceDeactivate)
		{
			try
			{
				IBaseWindow curBase = ((forceDeactivate) ? null : this.GetCurrentFile());
				IChartWindow curChart = ((this.ActiveMdiChild is IChartWindow) ? (this.ActiveMdiChild as IChartWindow) : null);

				IWorkWindow workWin = this.GetWorkWindow();

				GEDCOMRecordType rt = (curBase == null) ? GEDCOMRecordType.rtNone : curBase.GetSelectedRecordType();
				bool baseEn = (rt != GEDCOMRecordType.rtNone);

				this.miFileSave.Enabled = baseEn || (curChart != null);
				this.tbFileSave.Enabled = this.miFileSave.Enabled;
				this.miRecordAdd.Enabled = baseEn;
				this.tbRecordAdd.Enabled = this.miRecordAdd.Enabled;
				this.miRecordEdit.Enabled = baseEn;
				this.tbRecordEdit.Enabled = this.miRecordEdit.Enabled;
				this.miRecordDelete.Enabled = baseEn;
				this.tbRecordDelete.Enabled = this.miRecordDelete.Enabled;
				this.miStats.Enabled = baseEn;
				this.tbStats.Enabled = this.miStats.Enabled;

				this.miFilter.Enabled = (workWin != null && workWin.AllowFilter());
				this.tbFilter.Enabled = this.miFilter.Enabled;

				this.miSearch.Enabled = (workWin != null && workWin.AllowQuickFind());

				this.tbDocPrint.Enabled = (curChart != null && curChart.AllowPrint());
				this.tbDocPreview.Enabled = (curChart != null && curChart.AllowPrint());

				this.miTreeTools.Enabled = baseEn;
				this.miExportToFamilyBook.Enabled = baseEn;
				this.miExportToExcelFile.Enabled = baseEn;
				this.miFileClose.Enabled = baseEn;
				this.miFileProperties.Enabled = baseEn;
				this.miOrganizer.Enabled = baseEn;
				this.miSlideshow.Enabled = baseEn;
				this.miScripts.Enabled = baseEn;

				bool indivEn = baseEn && rt == GEDCOMRecordType.rtIndividual;

				this.miTreeAncestors.Enabled = indivEn;
				this.tbTreeAncestors.Enabled = this.miTreeAncestors.Enabled;
				this.miTreeDescendants.Enabled = indivEn;
				this.tbTreeDescendants.Enabled = this.miTreeDescendants.Enabled;
				this.miTreeBoth.Enabled = indivEn;
				this.tbTreeBoth.Enabled = this.miTreeBoth.Enabled;
				this.miPedigree.Enabled = indivEn;
				this.tbPedigree.Enabled = this.miPedigree.Enabled;
				this.miPedigree_dAboville.Enabled = indivEn;
				this.miPedigree_Konovalov.Enabled = indivEn;

				if (workWin != null) {
					this.StatusBar.Panels[0].Text = workWin.GetStatusString();
				}

				this.UpdateNavControls();

				this.StatusBar.Invalidate();
			} catch (Exception ex) {
				this.LogWrite("TfmGEDKeeper.UpdateControls(): " + ex.Message);
			}
		}

		public string GetCurrentFileName()
		{
			IBaseWindow cb = this.GetCurrentFile();
			string result = ((cb == null) ? "" : cb.Tree.FileName);
			return result;
		}

		public IBaseWindow FindBase(string fileName)
		{
			IBaseWindow result = null;

			int num = base.MdiChildren.Length;
			for (int i = 0; i < num; i++) {
				Form child = base.MdiChildren[i];

				if (child is IBaseWindow) {
					IBaseWindow baseWin = (child as IBaseWindow);

					if (string.Equals(baseWin.Tree.FileName, fileName)) {
						result = baseWin;
						break;
					}
				}
			}

			return result;
		}

		public IBaseWindow CreateBase(string fileName)
		{
			IBaseWindow result = this.FindBase(fileName);
			if (result != null) {
				(result as TfmBase).Activate();
				return result;
			}

			result = new TfmBase();
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
				for (int i = 0; i < num; i++) {
					Form child = base.MdiChildren[i];

					if (child is IBaseWindow) {
						GEDCOMTree tree = (child as IBaseWindow).Tree;

						string rfn = Path.ChangeExtension(tree.FileName, ".restore");
						tree.SaveToFile(rfn, this.fOptions.DefCharacterSet);
					}
				}
			} catch (Exception ex) {
				this.LogWrite("TfmGEDKeeper.CriticalSave(): " + ex.Message);
			}
		}

		private void miExportToFamilyBookClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			using (FamilyBookExporter fb = new FamilyBookExporter(curBase)) {
				fb.Generate(true);
			}
		}

		private void miExportToExcelFileClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			using (ExcelExporter exExp = new ExcelExporter(curBase)) {
				exExp.Options = this.fOptions;
				exExp.SelectedRecords = curBase.GetContentList(GEDCOMRecordType.rtIndividual);
				exExp.AppMode = false;
				exExp.Generate(true);
			}
		}

		private void miFilePropertiesClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			using (TfmFileProperties dlgFileProps = new TfmFileProperties(curBase)) {
				TfmGEDKeeper.Instance.ShowModalEx(dlgFileProps, false);
			}
		}

		private void miScriptsClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			using (TfmScriptDaemon dmn = new TfmScriptDaemon(curBase)) {
				TfmGEDKeeper.Instance.ShowModalEx(dmn, false);
			}
		}

		private void miTreeToolsClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			using (TfmTreeTools fmTreeTools = new TfmTreeTools(curBase)) {
				TfmGEDKeeper.Instance.ShowModalEx(fmTreeTools, false);
			}
		}

		private void miOptionsClick(object sender, EventArgs e)
		{
			TfmOptions fmOptions = new TfmOptions(this);
			try
			{
				/*GlobalOptions options = TfmGEDKeeper.Instance.Options;
				
				IBase xbase = this.GetCurrentFile();
				if (xbase != null) {
					IListManager listman = xbase.GetRecordsListManByType(GEDCOMRecordType.rtIndividual);
					listman.ListColumns.CopyTo(options.IndividualListColumns);
				}*/

				Form activeForm = this.ActiveMdiChild;
				if (activeForm is IBaseWindow) fmOptions.SetPage(OptionsPage.opInterface);
				if (activeForm is IChartWindow) fmOptions.SetPage(OptionsPage.opTreeChart);

				if (fmOptions.ShowDialog() == DialogResult.OK) {
					int num = base.MdiChildren.Length;
					for (int i = 0; i < num; i++) {
						Form child = base.MdiChildren[i];

						if (child is IBaseWindow) {
							(child as IBaseWindow).RefreshLists(true);
						} else if (child is IChartWindow) {
							(child as IChartWindow).GenChart(false);
						}
					}
				}
			} finally {
				fmOptions.Dispose();
			}
		}

		private void miFileCloseClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			curBase.Close();
		}

		private void miFileNewClick(object sender, EventArgs e)
		{
			this.CreateBase("");
		}

		private void miFileLoadClick(object sender, EventArgs e)
		{
			this.OpenDialog1.InitialDirectory = this.fOptions.LastDir;
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK) {
				this.CreateBase(this.OpenDialog1.FileName);
			}
		}

		public void miFileSaveClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile(true);
			if (curBase == null) return;

			this.SaveDialog1.FileName = curBase.Tree.FileName;
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK) {
				curBase.FileSave(this.SaveDialog1.FileName);
			}
		}

		private void miRecordAddClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			curBase.RecordAdd();
		}

		private void miRecordEditClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			curBase.RecordEdit(sender, e);
		}

		private void miRecordDeleteClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			curBase.RecordDelete();
		}

		private void miSearchClick(object sender, EventArgs e)
		{
			IWorkWindow win = this.GetWorkWindow();
			if (win == null) return;

			win.QuickFind();
		}

		private void miFilterClick(object sender, EventArgs e)
		{
			IWorkWindow win = this.GetWorkWindow();
			if (win == null) return;

			win.SetFilter();
		}

		private void tbPrevClick(object sender, EventArgs e)
		{
			IWorkWindow win = this.GetWorkWindow();
			if (win == null) return;

			win.NavPrev();
		}

		private void tbNextClick(object sender, EventArgs e)
		{
			IWorkWindow win = this.GetWorkWindow();
			if (win == null) return;

			win.NavNext();
		}

		private void tbDocPrintClick(object sender, EventArgs e)
		{
			IChartWindow chartWin = this.GetWorkWindow() as IChartWindow;
			if (chartWin != null && chartWin.AllowPrint()) {
				chartWin.DoPrint();
			}
		}

		private void tbDocPreviewClick(object sender, EventArgs e)
		{
			IChartWindow chartWin = this.GetWorkWindow() as IChartWindow;
			if (chartWin != null && chartWin.AllowPrint()) {
				chartWin.DoPrintPreview();
			}
		}

		private void miMapClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;
			
			TfmMaps frm_maps = new TfmMaps(curBase);
			frm_maps.MdiParent = this;
			frm_maps.Show();
		}

		private void miOrganizerClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			using (TfmOrganizer dlg = new TfmOrganizer(curBase)) {
				TfmGEDKeeper.Instance.ShowModalEx(dlg, false);
			}
		}

		private void miSlideshowClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			TfmSlideshow dlg = new TfmSlideshow(curBase);
			dlg.MdiParent = this;
			dlg.Show();
		}

		private void miStatsClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			List<GEDCOMRecord> selectedRecords = curBase.GetContentList(GEDCOMRecordType.rtIndividual);

			TfmStats fmStats = new TfmStats(curBase, selectedRecords);
			fmStats.Show();
		}

		private void miPedigree_dAbovilleClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			using (PedigreeExporter p = new PedigreeExporter(curBase)) {
				p.Ancestor = curBase.GetSelectedPerson();
				p.Options = this.fOptions;
				p.ShieldState = curBase.ShieldState;
				p.Kind = PedigreeExporter.PedigreeKind.pk_dAboville;
				p.Generate(true);
			}
		}

		private void miPedigree_KonovalovClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			using (PedigreeExporter p = new PedigreeExporter(curBase)) {
				p.Ancestor = curBase.GetSelectedPerson();
				p.Options = this.fOptions;
				p.ShieldState = curBase.ShieldState;
				p.Kind = PedigreeExporter.PedigreeKind.pk_Konovalov;
				p.Generate(true);
			}
		}

		private void miTreeAncestorsClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			if (TfmChart.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartBox.ChartKind.ckAncestors)) {
				TfmChart fmChart = new TfmChart(curBase, curBase.GetSelectedPerson());
				fmChart.ChartKind = TreeChartBox.ChartKind.ckAncestors;
				fmChart.GenChart(true);
			}
		}

		private void miTreeDescendantsClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			if (TfmChart.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartBox.ChartKind.ckDescendants)) {
				TfmChart fmChart = new TfmChart(curBase, curBase.GetSelectedPerson());
				fmChart.ChartKind = TreeChartBox.ChartKind.ckDescendants;
				fmChart.GenChart(true);
			}
		}

		private void miTreeBothClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			if (TfmChart.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartBox.ChartKind.ckBoth)) {
				TfmChart fmChart = new TfmChart(curBase, curBase.GetSelectedPerson());
				fmChart.ChartKind = TreeChartBox.ChartKind.ckBoth;
				fmChart.GenChart(true);
			}
		}

		private void miAncestorsCircleClick(object sender, EventArgs e)
		{
			IBaseWindow curBase = this.GetCurrentFile();
			if (curBase == null) return;

			TfmAncestorsCircle fmChart = new TfmAncestorsCircle(curBase, curBase.GetSelectedPerson());
			fmChart.GenChart(true);
		}

		#endregion

		#region Help and Windows

		private void miLogSendClick(object sender, EventArgs e)
		{
			if (File.Exists(this.fLogFilename)) {
				MapiMailMessage message = new MapiMailMessage("GEDKeeper: error notification", "This automatic notification of error.");
				message.Recipients.Add(GKData.AppMail);
				message.Files.Add(this.fLogFilename);
				message.ShowDialog();
			}
		}

		private void miLogViewClick(object sender, EventArgs e)
		{
			SysUtils.LoadExtFile(this.fLogFilename);
		}

		private void miAboutClick(object sender, EventArgs e)
		{
			TfmAbout.ShowAbout();
		}

		public void ShowHelpTopic(string topic)
		{
			string fns = GKUtils.GetAppPath() + "GEDKeeper2.chm" + topic;
			SysUtils.HtmlHelp(this.Handle, fns, 0u, 0u);
		}

		private void miGenResourcesClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhGenRes.html");
		}

		private void miKinshipTermsClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhRelations.html");
		}

		private void miFAQClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhFAQ.html");
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
			for (int i = base.MdiChildren.Length - 1; i >= 0; i--) {
				base.MdiChildren[i].WindowState = FormWindowState.Minimized;
			}
		}

		private void miWinArrangeClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.ArrangeIcons);
		}

		#endregion

		#region ILocalization implementation

		public void SetLang()
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
			this.miSlideshow.Text = LangMan.LS(LSID.LSID_Slideshow) + "...";
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
			this.miLogView.Text = LangMan.LS(LSID.LSID_LogView);
			this.miPlugins.Text = LangMan.LS(LSID.LSID_Plugins);

			this.miSearch.Text = LangMan.LS(LSID.LSID_Search);
			this.miAncestorsCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);

			this.tbDocPrint.ToolTipText = LangMan.LS(LSID.LSID_DocPrint);
			this.tbDocPreview.ToolTipText = LangMan.LS(LSID.LSID_DocPreview);

        	this.tbPrev.ToolTipText = LangMan.LS(LSID.LSID_PrevRec);
        	this.tbNext.ToolTipText = LangMan.LS(LSID.LSID_NextRec);
		}

		#endregion

		#region Plugins support

		public PluginInfo GetPluginAttributes(IPlugin plugin)
		{
			if (plugin == null) {
				throw new ArgumentNullException("plugin");
			}

			PluginInfo info = new PluginInfo();

			Assembly asm = plugin.GetType().Assembly;

			var attr1 = GKUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(asm);
			if (attr1 != null) info.Title = attr1.Title;

			var attr2 = GKUtils.GetAssemblyAttribute<AssemblyDescriptionAttribute>(asm);
			if (attr2 != null) info.Description = attr2.Description;

			var attr3 = GKUtils.GetAssemblyAttribute<AssemblyCopyrightAttribute>(asm);
			if (attr3 != null) info.Copyright = attr3.Copyright;

			var attr4 = GKUtils.GetAssemblyAttribute<AssemblyFileVersionAttribute>(asm);
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

			this.fActiveWidgets.Clear();

			int num = this.fPlugins.Count;
			for (int i = 0; i < num; i++) {
				IPlugin plugin = this.fPlugins[i];
				string dispName = plugin.DisplayName;

				MenuItem mi = new GKMenuItem(dispName, i);
				mi.Click += this.Plugin_Click;
				mi.Tag = plugin;
				this.miPlugins.MenuItems.Add(mi);

				if (plugin is IWidget) {
					WidgetInfo widInfo = new WidgetInfo();
					widInfo.Widget = (plugin as IWidget);
					widInfo.MenuItem = mi;
					this.fActiveWidgets.Add(widInfo);

					(plugin as IWidget).WidgetInit(this);
				}
			}
		}

		private void UnloadPlugins()
		{
			if (this.fPlugins == null) return;

			int num = this.fPlugins.Count;
			for (int i = 0; i < num; i++) {
				IPlugin plugin = this.fPlugins[i];
				plugin.Shutdown();
			}
		}

		private void LoadPlugins(string path)
		{
			AppDomain.CurrentDomain.SetupInformation.PrivateBinPath = path;

			this.fPlugins = new List<IPlugin>();

			Type pluginType = typeof(IPlugin);
			string[] pluginFiles = Directory.GetFiles(path, "*.dll");

			foreach (string pfn in pluginFiles)
			{
				try
				{
					Assembly asm;

					try
					{
                        AssemblyName assemblyName = AssemblyName.GetAssemblyName(pfn);
                        asm = Assembly.Load(assemblyName);
					} catch {
						asm = null;
						// block exceptions for bad or non-dotnet assemblies
					}

					if (asm == null) continue;

					Type[] types = asm.GetTypes();
					foreach (Type type in types)
					{
						if (type.IsInterface || type.IsAbstract) continue;
						if (type.GetInterface(pluginType.FullName) == null) continue;

						IPlugin plugin = (IPlugin)Activator.CreateInstance(type);
						plugin.Startup(this);
						this.fPlugins.Add(plugin);
					}
				} catch (Exception ex) {
					this.LogWrite("TfmGEDKeeper.LoadPlugins(" + pfn + "): " + ex.Message);
				}
			}
		}

		#endregion

		#region IHost implementation

		public ILangMan CreateLangMan(object sender)
		{
			if (sender == null) {
				return null;
			}

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

		public IBaseWindow GetCurrentFile(bool extMode = false)
		{
			IChartWindow curChart = ((this.ActiveMdiChild is IChartWindow) ? (this.ActiveMdiChild as IChartWindow) : null);
			IBaseWindow result;

			if (extMode && curChart != null) {
				result = curChart.Base;
			} else {
				result = ((base.ActiveMdiChild is IBaseWindow) ? (base.ActiveMdiChild as IBaseWindow) : null);
			}

			return result;
		}

		public IWorkWindow GetWorkWindow()
		{
			Form activeForm = this.ActiveMdiChild;
			return (activeForm is IWorkWindow) ? activeForm as IWorkWindow : null;
		}

		public void LogWrite(string msg)
		{
			Logger.LogWrite(msg);
		}

		public void NotifyRecord(IBaseWindow aBase, object record, RecordAction action)
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

		private WidgetInfo FindWidgetInfo(IWidget widget)
		{
			foreach (WidgetInfo widgetInfo in this.fActiveWidgets) {
				if (widgetInfo.Widget == widget) {
					return widgetInfo;
				}
			}

			return null;
		}

		public void WidgetShow(IWidget widget)
		{
			WidgetInfo widInfo = this.FindWidgetInfo(widget);
			if (widInfo == null) return;

			if (widInfo.MenuItem != null) widInfo.MenuItem.Checked = true;
		}

		public void WidgetClose(IWidget widget)
		{
			WidgetInfo widInfo = this.FindWidgetInfo(widget);
			if (widInfo == null) return;

			if (widInfo.MenuItem != null) widInfo.MenuItem.Checked = false;
		}

		public bool IsWidgetActive(IWidget widget)
		{
			WidgetInfo widInfo = this.FindWidgetInfo(widget);
			if (widInfo == null || widInfo.MenuItem == null) {
				return false;
			} else {
				return widInfo.MenuItem.Checked;
			}
		}

		public void BaseChanged(IBaseWindow aBase)
		{
			foreach (WidgetInfo widgetInfo in this.fActiveWidgets) {
				widgetInfo.Widget.BaseChanged(aBase);
			}
		}

		public void BaseClosed(IBaseWindow aBase)
		{
			foreach (WidgetInfo widgetInfo in this.fActiveWidgets) {
				widgetInfo.Widget.BaseClosed(aBase);
			}
		}

		public void ShowMDI(Form form)
		{
			if (form != null) {
				form.MdiParent = this;
				form.Show();
			}
		}

		public void EnableWindow(Form form, bool value)
		{
			if (form != null) {
        		SysUtils.EnableWindow(form.Handle, value);
        	}
		}

		#endregion

		#region ISingleInstanceEnforcer implementation

		void ISingleInstanceEnforcer.OnMessageReceived(MessageEventArgs e)
		{
			OnMessageReceivedInvoker invoker = delegate(MessageEventArgs eventArgs) {
				string msg = eventArgs.Message as string;

				if (!string.IsNullOrEmpty(msg)) {
					MessageBox.Show(msg, "Message from new instance", MessageBoxButtons.OK, MessageBoxIcon.Information);
				} else {
					//MessageBox.Show("A non-textual message has been received.", "Message From New Instance", MessageBoxButtons.OK, MessageBoxIcon.Information);

					string[] args = eventArgs.Message as string[];

					if (args != null) {
						this.SetArgs(args);
						this.LoadArgs();
					}
				}
			};

			if (InvokeRequired) {
				Invoke(invoker, e);
			} else {
				invoker(e);
			}
		}

		void ISingleInstanceEnforcer.OnNewInstanceCreated(EventArgs e)
		{
			//MessageBox.Show("New instance of the program has been created.", "Notification from new instance", MessageBoxButtons.OK, MessageBoxIcon.Information);
		}

		#endregion
	}
}
