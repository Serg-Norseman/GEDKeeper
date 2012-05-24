using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmGEDKeeper : Form, ILocalization
	{
		private NamesTable FNamesTable;
		private GlobalOptions FOptions;
		private string[] FCommandArgs;
		private SearchManager FSearchMan;

		public TfmTimeLine fmTimeLine;
		public TfmCalendar fmCalendar;
		public TfmNamesBook fmNamesBook;
		public TfmCalcWidget fmCalcWidget;

		private static TfmGEDKeeper FInstance = null;

		public static TfmGEDKeeper Instance
		{
			get { return FInstance; }
		}

		public NamesTable NamesTable
		{
			get { return this.FNamesTable; }
		}

		public GlobalOptions Options
		{
			get { return this.FOptions; }
		}

		public SearchManager SearchMan
		{
			get { return FSearchMan; }
		}

		public TfmGEDKeeper(string[] args)
		{
			this.InitializeComponent();
			GKMapBrowser.GeoInit();
			FInstance = this;
			FSearchMan = new SearchManager();

			FCommandArgs = (string[])args.Clone();
		}

		private void FormCreate(object sender, EventArgs e)
		{
			SysUtils.LogInit(TGenEngine.GetAppDataPath() + "GEDKeeper2.log");

			this.FOptions = new GlobalOptions();
			this.FOptions.LoadFromFile(TGenEngine.GetAppDataPath() + "GEDKeeper2.ini");
			this.FOptions.FindLanguages();

			if (this.FOptions.MWinRect.Left != -1 && this.FOptions.MWinRect.Top != -1 && this.FOptions.MWinRect.Right != -1 && this.FOptions.MWinRect.Bottom != -1)
			{
				base.Left = this.FOptions.MWinRect.Left;
				base.Top = this.FOptions.MWinRect.Top;
				base.Width = this.FOptions.MWinRect.Right;
				base.Height = this.FOptions.MWinRect.Bottom;
			}
			else
			{
				base.Left = (Screen.PrimaryScreen.WorkingArea.Width - 800) / 2;
				base.Top = (Screen.PrimaryScreen.WorkingArea.Height - 600) / 2;
				base.Width = 800;
				base.Height = 600;
			}
			base.WindowState = this.FOptions.MWinState;

			this.FNamesTable = new NamesTable();
			this.FNamesTable.LoadFromFile(TGenEngine.GetAppDataPath() + "GEDKeeper2.nms");

			this.LoadLanguage((int)this.FOptions.InterfaceLang);

			this.UpdateMRU();
			this.UpdateControls(false);

			if (FCommandArgs.Length > 0) {
				this.CreateBase(FCommandArgs[0]);
			}
		}

		void TfmGEDKeeperFormClosed(object sender, FormClosedEventArgs e)
		{
			this.FOptions.MWinRect = this.GetFormRect(this);
			this.FOptions.MWinState = base.WindowState;

			SysUtils.HtmlHelp(IntPtr.Zero, null, 18u, 0u);

			this.FNamesTable.SaveToFile(TGenEngine.GetAppDataPath() + "GEDKeeper2.nms");
			this.FNamesTable.Dispose();

			this.FOptions.SaveToFile(TGenEngine.GetAppDataPath() + "GEDKeeper2.ini");
			this.FOptions.Dispose();
		}

		private void MRUFileClick(object sender, EventArgs e)
		{
			int idx = ((GKMenuItem)sender).Tag;
			this.CreateBase(this.FOptions.MRUFiles[idx].FileName);
		}

		private void UpdateMRU()
		{
			this.miMRUFiles.Enabled = (this.FOptions.MRUFiles.Count > 0);
			this.miMRUFiles.MenuItems.Clear();
			this.MenuMRU.MenuItems.Clear();

			int num = this.FOptions.MRUFiles.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string fn = this.FOptions.MRUFiles[i].FileName;

				MenuItem mi = new GKMenuItem(fn, i);
				mi.Click += new EventHandler(this.MRUFileClick);
				this.miMRUFiles.MenuItems.Add(mi);

				mi = new GKMenuItem(fn, i);
				mi.Click += new EventHandler(this.MRUFileClick);
				this.MenuMRU.MenuItems.Add(mi);
			}
		}

		private TRect GetFormRect(Form aForm)
		{
			int x = aForm.Left;
			int y = aForm.Top;
			int w = aForm.Width;
			int h = aForm.Height;
			Screen scr = Screen.PrimaryScreen;
			int mw = scr.WorkingArea.Width;
			int mh = scr.WorkingArea.Height;
			if (x < 0) x = 0;
			if (y < 0) y = 0;
			if (w > mw) w = mw;
			if (h > mh) h = mh;
			return TRect.Create(x, y, x + w - 1, y + h - 1);
		}

		private void TfmGEDKeeper_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.F12)
			{
				// dummy
			}
		}

		private void StatusBarDblClick(object sender, EventArgs e)
		{
		}

		private void miExitClick(object sender, EventArgs e)
		{
			base.Close();
		}

		private void miExportToWebClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ExportToWeb();
			}
		}

		private void miExportToExcelFileClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ExportToExcel(false);
			}
		}

		private void miExportToPDFFileClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ExportToPDF();
			}
		}

		private void miFilePropertiesClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.FileProperties();
			}
		}

		private void miStreamInputClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.PersonScan();
			}
		}

		private void miScriptsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowScriptDaemon();
			}
		}

		private void miTreeToolsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowTreeTools();
			}
		}

		private void miOptionsClick(object sender, EventArgs e)
		{
			TfmOptions fmOptions = new TfmOptions();
			try
			{
				Form active_form = this.ActiveMdiChild;
				if (active_form is TfmBase) fmOptions.SetPage(OptionsPage.opInterface);
				if (active_form is TfmChart) fmOptions.SetPage(OptionsPage.opTreeChart);
				if (active_form is TfmAncestorsCircle) fmOptions.SetPage(OptionsPage.opAncestorsCircle);

				if (fmOptions.ShowDialog() == DialogResult.OK)
				{
					int num = base.MdiChildren.Length;
					for (int i = 0; i <= num; i++)
					{
						Form child = base.MdiChildren[i];

						if (child is TfmBase) {
							(child as TfmBase).ListsRefresh(true);
						} else if (child is TfmChart) {
							(child as TfmChart).GenChart(false);
						} else if (child is TfmAncestorsCircle) {
							(child as TfmAncestorsCircle).Invalidate();
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
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null) cur_base.Close();
		}

		private void miMapClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null) cur_base.ShowMap();
		}

		private void miAncestorsCircleClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null) cur_base.ShowAncestorsCircle();
		}

		private void miOrganizerClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null) cur_base.ShowOrganizer();
		}

		private void miTimeLineClick(object sender, EventArgs e)
		{
			if (!this.miTimeLine.Checked)
			{
				this.fmTimeLine = new TfmTimeLine();
				this.fmTimeLine.Location = new Point(10, Screen.PrimaryScreen.WorkingArea.Height - this.fmTimeLine.Height - 10);
				this.fmTimeLine.Show();
				this.miTimeLine.Checked = true;
			}
			else
			{
				this.miTimeLine.Checked = false;
				this.fmTimeLine.Dispose();
				this.fmTimeLine = null;
			}
		}

		private void miCalendarClick(object sender, EventArgs e)
		{
			if (!this.miCalendar.Checked)
			{
				this.fmCalendar = new TfmCalendar();
				this.fmCalendar.Location = new Point(Screen.PrimaryScreen.WorkingArea.Width - this.fmCalendar.Width - 10, 50);
				this.fmCalendar.Show();
				this.miCalendar.Checked = true;
			}
			else
			{
				this.miCalendar.Checked = false;
				this.fmCalendar.Dispose();
				this.fmCalendar = null;
			}
		}

		private void miNamesBookClick(object sender, EventArgs e)
		{
			if (!this.miNamesBook.Checked)
			{
				Screen scr = Screen.PrimaryScreen;
				this.fmNamesBook = new TfmNamesBook();
				this.fmNamesBook.Location = new Point(scr.WorkingArea.Width - this.fmNamesBook.Width - 10, (scr.WorkingArea.Height - this.fmNamesBook.Height) / 2);
				this.fmNamesBook.Show();
				this.miNamesBook.Checked = true;
			}
			else
			{
				this.miNamesBook.Checked = false;
				this.fmNamesBook.Dispose();
				this.fmNamesBook = null;
			}
		}

		private void miCalcClick(object sender, EventArgs e)
		{
			if (!this.miCalc.Checked)
			{
				Screen scr = Screen.PrimaryScreen;
				this.fmCalcWidget = new TfmCalcWidget();
				this.fmCalcWidget.Location = new Point(scr.WorkingArea.Width - this.fmCalcWidget.Width - 10, scr.WorkingArea.Height - this.fmCalcWidget.Height - 10);
				this.fmCalcWidget.Show();
				this.miCalc.Checked = true;
			}
			else
			{
				this.miCalc.Checked = false;
				this.fmCalcWidget.Dispose();
				this.fmCalcWidget = null;
			}
		}

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

		private void miFilterClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.SetFilter();
			}
		}

		private void miSearchClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				TfmTextSearch ts_dlg = new TfmTextSearch(cur_base);
				ts_dlg.MdiParent = this;
				ts_dlg.Show();
			}
		}

		private void tbPrevClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.NavPrev();
			}
		}

		private void tbNextClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.NavNext();
			}
		}

		private void miStatsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowStats();
			}
		}

		private void miFileNewClick(object sender, EventArgs e)
		{
			this.CreateBase("");
		}

		private void miFileLoadClick(object sender, EventArgs e)
		{
			this.OpenDialog1.InitialDirectory = this.FOptions.LastDir;
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				this.CreateBase(this.OpenDialog1.FileName);
			}
		}

		private void miUndoClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.DoUndo();
			}
		}

		private void miRedoClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.DoRedo();
			}
		}

		private void miTreeAncestorsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowTreeAncestors();
			}
		}

		private void miTreeDescendantsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowTreeDescendants();
			}
		}

		private void miPedigree_dAbovilleClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.GenPedigree_dAboville();
			}
		}

		private void miPedigree_KonovalovClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.GenPedigree_Konovalov();
			}
		}

		private void miRecordAddClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.RecordAdd();
			}
		}

		private void miRecordEditClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.RecordEdit(sender, e);
			}
		}

		private void miRecordDeleteClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.RecordDelete();
			}
		}

		private void miTreeBothClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowTreeBoth();
			}
		}

		private void FormShow(object sender, EventArgs e)
		{
			int num = this.FOptions.LastBasesCount - 1;
			for (int i = 0; i <= num; i++)
			{
				string lb = this.FOptions.GetLastBase(i);
				if (File.Exists(lb))
				{
					TfmBase base_win = this.CreateBase(lb);
				}
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
			} else if (e.Button == this.tbUndo) {
				this.miUndoClick(null, null);
			} else if (e.Button == this.tbRedo) {
				this.miRedoClick(null, null);
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

		private void TfmGEDKeeper_Resize(object sender, EventArgs e)
		{
			this.StatusBar.Panels[0].Width = base.Width - 50;
		}

		void TfmGEDKeeperFormClosing(object sender, FormClosingEventArgs e)
		{
			this.FOptions.ClearLastBases();
			for (int i = base.MdiChildren.Length - 1; i >= 0; i--)
			{
				Form mdi_child = base.MdiChildren[i];
				if (mdi_child is TfmBase)
				{
					this.FOptions.AddLastBase((mdi_child as TfmBase).FileName);
				}
			}
		}

		private void TfmGEDKeeper_DragEnter(object sender, DragEventArgs e)
		{
			if (e.Data.GetDataPresent(DataFormats.FileDrop)) {
				e.Effect = DragDropEffects.Copy;
			} else {
				e.Effect = DragDropEffects.None;
			}
		}

		private void TfmGEDKeeper_DragDrop(object sender, DragEventArgs e)
		{
			try
			{
				Array a = e.Data.GetData(DataFormats.FileDrop) as Array;
				if (a != null)
				{
					for (int i = 0; i <= a.Length - 1; i++)
					{
						string fn = a.GetValue(i).ToString();
						this.CreateBase(fn);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKMain.TfmGEDKeeper_DragDrop(): " + E.Message);
			}
		}

		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);
			if (m.Msg == 1135)
			{
				if (this.fmCalcWidget != null && this.fmCalcWidget.Visible)
				{
					SysUtils.EnableWindow((uint)this.fmCalcWidget.Handle.ToInt32(), true);
				}
			}
			else
			{
				int arg_4F_0 = m.Msg;
			}
		}

		public TfmBase GetCurrentFile()
		{
			TfmBase result = ((base.ActiveMdiChild is TfmBase) ? (base.ActiveMdiChild as TfmBase) : null);
			return result;
		}

		public string GetCurrentFileName()
		{
			TfmBase cb = this.GetCurrentFile();
			string result = ((cb == null) ? "" : cb.FileName);
			return result;
		}

		public void AddMRU(string aFileName)
		{
			int idx = this.FOptions.MRUFiles_IndexOf(aFileName);
			if (idx >= 0)
			{
				GlobalOptions.TMRUFile tmp_mf;
				tmp_mf = this.FOptions.MRUFiles[0];
				this.FOptions.MRUFiles[0] = this.FOptions.MRUFiles[idx];
				this.FOptions.MRUFiles[idx] = tmp_mf;
			}
			else
			{
				GlobalOptions.TMRUFile new_mf = new GlobalOptions.TMRUFile();
				new_mf.FileName = aFileName;
				this.FOptions.MRUFiles.Insert(0, new_mf);
			}

			this.UpdateMRU();
		}

		public void CheckMRUWin(string aFileName, Form frm)
		{
			int idx = this.FOptions.MRUFiles_IndexOf(aFileName);
			if (idx >= 0)
			{
				GlobalOptions.TMRUFile mf = this.FOptions.MRUFiles[idx];
				
				mf.WinRect = this.GetFormRect(frm);
				mf.WinState = frm.WindowState;
			}
		}

		public TfmBase CreateBase(string aFileName)
		{
			TfmBase result = new TfmBase();
			result.MdiParent = this;
			result.Show();

			if (aFileName != "" && File.Exists(aFileName)) {
				result.FileLoad(aFileName);
			} else {
				result.FileNew();
			}

			int idx = this.FOptions.MRUFiles_IndexOf(aFileName);
			if (idx >= 0)
			{
				GlobalOptions.TMRUFile mf = this.FOptions.MRUFiles[idx];
				SetFormRect(result, mf.WinRect, mf.WinState);
			}

			return result;
		}

		private void SetFormRect(Form aForm, TRect rt, FormWindowState winState)
		{
			// check for new and empty struct
			if (!rt.IsEmpty())
			{
				aForm.Left = rt.Left;
				aForm.Top = rt.Top;
				aForm.Width = rt.GetWidth();
				aForm.Height = rt.GetHeight();
				aForm.WindowState = winState;
			}
		}

		public void UpdateControls(bool ForceDeactivate)
		{
			try
			{
				TfmBase cur_base = ((ForceDeactivate) ? null : this.GetCurrentFile());
				TfmChart cur_chart = ((this.ActiveMdiChild is TfmChart) ? (this.ActiveMdiChild as TfmChart) : null);


				TGEDCOMRecordType rt;
				bool base_en;
				if (cur_base == null)
				{
					rt = TGEDCOMRecordType.rtNone;
					base_en = false;
				}
				else
				{
					rt = (TGEDCOMRecordType)(cur_base.PageRecords.SelectedIndex + 1);
					base_en = true;
				}

				this.miFileClose.Enabled = base_en;
				this.miFileSave.Enabled = base_en;
				this.tbFileSave.Enabled = this.miFileSave.Enabled;
				this.miFileProperties.Enabled = base_en;
				this.miExportToWeb.Enabled = base_en;
				this.miExportToExcelFile.Enabled = base_en;
				this.miExportToPDFFile.Enabled = base_en;
				this.miTreeTools.Enabled = base_en;
				this.miStreamInput.Enabled = base_en;
				this.miRecordAdd.Enabled = base_en;
				this.tbRecordAdd.Enabled = this.miRecordAdd.Enabled;
				this.miRecordEdit.Enabled = base_en;
				this.tbRecordEdit.Enabled = this.miRecordEdit.Enabled;
				this.miRecordDelete.Enabled = base_en;
				this.tbRecordDelete.Enabled = this.miRecordDelete.Enabled;
				bool indiv_en = base_en && rt == TGEDCOMRecordType.rtIndividual;
				this.miFilter.Enabled = indiv_en;
				this.tbFilter.Enabled = this.miFilter.Enabled;
				this.miSearch.Enabled = indiv_en;
				this.miStats.Enabled = base_en;
				this.tbStats.Enabled = this.miStats.Enabled;
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
				this.miTimeLine.Enabled = base_en;
				this.miOrganizer.Enabled = base_en;
				this.miScripts.Enabled = base_en;
				this.tbPrev.Enabled = (cur_base != null && cur_base.Navman.CanBackward());
				this.tbNext.Enabled = (cur_base != null && cur_base.Navman.CanForward());
				bool test_funcs = TGenEngine.IsDevComp();
				this.miUndo.Enabled = (test_funcs && cur_base != null && cur_base.Undoman.CanUndo());
				this.tbUndo.Enabled = this.miUndo.Enabled;
				this.miRedo.Enabled = (test_funcs && cur_base != null && cur_base.Undoman.CanRedo());
				this.tbRedo.Enabled = this.miRedo.Enabled;

				if (cur_base != null) {
					this.StatusBar.Panels[0].Text = cur_base.GetStatusString();
				} else if (cur_chart != null) {
					this.StatusBar.Panels[0].Text = cur_chart.GetStatusString();
				}

				this.StatusBar.Invalidate();
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKMain.UpdateControls(): " + E.Message);
			}
		}

		public void ShowHelpTopic(string aTopic)
		{
			string fns = TGenEngine.GetAppPath() + "GEDKeeper2.chm" + aTopic;
			SysUtils.HtmlHelp(this.Handle, fns, 0u, 0u);
		}

		void ILocalization.SetLang()
		{
			this.miFile.Text = LangMan.LSList[0];
			this.miEdit.Text = LangMan.LSList[1];
			this.miPedigree.Text = LangMan.LSList[2];
			this.miService.Text = LangMan.LSList[3];
			this.miWindow.Text = LangMan.LSList[4];
			this.miHelp.Text = LangMan.LSList[5];
			this.miFileNew.Text = LangMan.LSList[6];
			this.miFileLoad.Text = LangMan.LSList[7];
			this.miMRUFiles.Text = LangMan.LSList[8];
			this.miFileSave.Text = LangMan.LSList[9];
			this.miFileClose.Text = LangMan.LSList[10];
			this.miFileProperties.Text = LangMan.LSList[11];
			this.miExport.Text = LangMan.LSList[12];
			this.miExportToWeb.Text = LangMan.LSList[13];
			this.miExportToExcelFile.Text = LangMan.LSList[15];
			this.miExit.Text = LangMan.LSList[16];
			this.miUndo.Text = LangMan.LSList[17];
			this.miRedo.Text = LangMan.LSList[18];
			this.miRecordAdd.Text = LangMan.LSList[19];
			this.miRecordEdit.Text = LangMan.LSList[20];
			this.miRecordDelete.Text = LangMan.LSList[21];
			this.miStreamInput.Text = LangMan.LSList[22] + "...";
			this.miTreeAncestors.Text = LangMan.LSList[23];
			this.miTreeDescendants.Text = LangMan.LSList[24];
			this.miTreeBoth.Text = LangMan.LSList[25];
			this.miPedigree_dAboville.Text = LangMan.LSList[26];
			this.miPedigree_Konovalov.Text = LangMan.LSList[27];
			this.miMap.Text = LangMan.LSList[28] + "...";
			this.miStats.Text = LangMan.LSList[29] + "...";
			this.miCalc.Text = LangMan.LSList[30] + "...";
			this.miNamesBook.Text = LangMan.LSList[31] + "...";
			this.miCalendar.Text = LangMan.LSList[32] + "...";
			this.miTimeLine.Text = LangMan.LSList[33] + "...";
			this.miOrganizer.Text = LangMan.LSList[34] + "...";
			this.miScripts.Text = LangMan.LSList[35];
			this.miTreeTools.Text = LangMan.LSList[37];
			this.miFilter.Text = LangMan.LSList[38] + "...";
			this.miOptions.Text = LangMan.LSList[39] + "...";
			this.miWinCascade.Text = LangMan.LSList[40];
			this.miWinHTile.Text = LangMan.LSList[41];
			this.miWinVTile.Text = LangMan.LSList[42];
			this.miWinMinimize.Text = LangMan.LSList[43];
			this.miWinArrange.Text = LangMan.LSList[44];
			this.miGenResources.Text = LangMan.LSList[45];
			this.miKinshipTerms.Text = LangMan.LSList[46];
			this.miFAQ.Text = LangMan.LSList[47];
			this.miContext.Text = LangMan.LSList[48];
			this.miAbout.Text = LangMan.LSList[49] + "...";
			this.miExportToPDFFile.Text = LangMan.LS(LSID.LSID_ExportToPDFFile);
			this.miAncestorsCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
			this.miLogSend.Text = LangMan.LS(LSID.LSID_LogSend);
			this.miSearch.Text = LangMan.LS(LSID.LSID_FullTextSearch);
		}

		public void LoadLanguage(int LangCode)
		{
			Screen scr = Screen.PrimaryScreen;
			if (LangCode != LangMan.LSDefCode)
			{
				bool loaded = false;

				int num = this.FOptions.LangsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					if ((int)this.FOptions.GetLang(i).Code == LangCode)
					{
						loaded = LangMan.LoadFromFile(this.FOptions.GetLang(i).FileName);
						break;
					}
				}

				if (!loaded) LangCode = LangMan.LSDefCode;
			}

			if (LangCode == LangMan.LSDefCode)
			{
				LangMan.DefInit();
			}

			int num2 = base.MdiChildren.Length - 1;
			for (int i = 0; i <= num2; i++)
			{
				Form child = base.MdiChildren[i];
				if (child is ILocalization) (child as ILocalization).SetLang();
			}
			(this as ILocalization).SetLang();

			this.FOptions.InterfaceLang = (ushort)LangCode;
		}

		public void miFileSaveClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				this.SaveDialog1.FileName = cur_base.FileName;
				if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
				{
					cur_base.FileSave(this.SaveDialog1.FileName);
				}
			}
		}

		public DialogResult ShowModalEx(Form aForm, Form aPopupParent, bool KeepModeless)
		{
			if (KeepModeless)
			{
				SysUtils.PostMessage((uint)GKUI.TfmGEDKeeper.Instance.Handle.ToInt32(), 1135u, 0, 0);
			}
			return aForm.ShowDialog();
		}

		void StatusBarDrawItem(object sender, StatusBarDrawItemEventArgs sbdevent)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null) {
				Bitmap pic = null;
				switch (cur_base.ShieldState) {
					case TGenEngine.TShieldState.ssNone:
						pic = (Bitmap)GKResources.iRGShieldNone.Clone();
						break;
					case TGenEngine.TShieldState.ssMiddle:
						pic = (Bitmap)GKResources.iRGShieldMid.Clone();
						break;
					case TGenEngine.TShieldState.ssMaximum:
						pic = (Bitmap)GKResources.iRGShieldMax.Clone();
						break;
				}

				if (pic != null) {
					pic.MakeTransparent(pic.GetPixel(0, 0));
					sbdevent.Graphics.DrawImage(pic, sbdevent.Bounds.Left, sbdevent.Bounds.Top);
				}
			}
		}

		void StatusBarPanelClick(object sender, StatusBarPanelClickEventArgs e)
		{
			if (e.StatusBarPanel == StatusBarPanel2 && e.Clicks == 2) {
				TfmBase cur_base = this.GetCurrentFile();
				if (cur_base == null) return;
				TGenEngine.TShieldState ss = cur_base.ShieldState;
				if (ss == TGenEngine.TShieldState.ssNone) {
					ss = TGenEngine.TShieldState.ssMaximum;
				} else {
					ss = (TGenEngine.TShieldState)((int)ss + 1);
				}
				cur_base.ShieldState = ss;
				StatusBar.Invalidate();
			}
		}
	}
}
