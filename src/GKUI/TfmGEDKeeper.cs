using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI
{
	public partial class TfmGEDKeeper : Form, ILocalization
	{
		private TNamesTable FNamesTable;
		private TGlobalOptions FOptions;
		private string[] FCommandArgs;

		public TfmTimeLine fmTimeLine;
		public TfmCalendar fmCalendar;
		public TfmNamesBook fmNamesBook;
		public TfmCalcWidget fmCalcWidget;

		private static TfmGEDKeeper FInstance = null;

		public static TfmGEDKeeper Instance
		{
			get { return FInstance; }
		}

		public TNamesTable NamesTable
		{
			get { return this.FNamesTable; }
		}

		public TGlobalOptions Options
		{
			get { return this.FOptions; }
		}

		public TfmGEDKeeper(string[] args)
		{
			this.InitializeComponent();
			TMapBrowser.GeoInit();
			FInstance = this;

			FCommandArgs = (string[])args.Clone();
		}

		private void FormCreate(object sender, EventArgs e)
		{
			SysUtils.LogInit(SysUtils.GetAppPath() + "GEDKeeper2.log");

			this.FOptions = new TGlobalOptions();
			this.FOptions.LoadFromFile(SysUtils.GetAppPath() + "GEDKeeper2.ini");
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

			this.FNamesTable = new TNamesTable();
			this.FNamesTable.LoadFromFile(SysUtils.GetAppPath() + "GEDKeeper2.nms");

			this.LoadLanguage((int)this.FOptions.InterfaceLang);

			this.UpdateMRU();
			this.UpdateControls(false);

			if (FCommandArgs.Length > 0) {
				this.CreateBase(FCommandArgs[0]);
			}
		}

		private void MRUFileClick(object sender, EventArgs e)
		{
			int idx = ((TGKMenuItem)sender).Tag;
			this.CreateBase(this.FOptions.MRUFiles[idx]);
		}

		private void UpdateMRU()
		{
			this.miMRUFiles.Enabled = (this.FOptions.MRUFiles.Count > 0);
			this.miMRUFiles.MenuItems.Clear();
			this.MenuMRU.MenuItems.Clear();

			int num = this.FOptions.MRUFiles.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				MenuItem mi = new TGKMenuItem(this.FOptions.MRUFiles[i], i);
				mi.Click += new EventHandler(this.MRUFileClick);
				this.miMRUFiles.MenuItems.Add(mi);

				mi = new TGKMenuItem(this.FOptions.MRUFiles[i], i);
				mi.Click += new EventHandler(this.MRUFileClick);
				this.MenuMRU.MenuItems.Add(mi);
			}
		}

		private TRect CheckFormRect(Form aForm)
		{
			int x = aForm.Left;
			int y = aForm.Top;
			int w = aForm.Width;
			int h = aForm.Height;
			Screen scr = Screen.PrimaryScreen;
			int mw = scr.WorkingArea.Width;
			int mh = scr.WorkingArea.Height;
			if (x < 0)
			{
				x = 0;
			}
			if (y < 0)
			{
				y = 0;
			}
			if (w > mw)
			{
				w = mw;
			}
			if (h > mh)
			{
				h = mh;
			}
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

		private void miFilePropertiesClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.FileProperties(TfmBase.TFilePropertiesMode.fpmAuthor);
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
				cur_base.TreeTools();
			}
		}

		private void miOptionsClick(object sender, EventArgs e)
		{
			TfmOptions fmOptions = new TfmOptions();
			try
			{
				if (fmOptions.ShowDialog() == DialogResult.OK)
				{
					Form[] mdiChildren = base.MdiChildren;
					int num = ((mdiChildren != null) ? mdiChildren.Length : 0) - 1;
					for (int i = 0; i <= num; i++)
					{
						if (base.MdiChildren[i] is TfmBase)
						{
							(base.MdiChildren[i] as TfmBase).ListsRefresh(true);
						}
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmOptions);
			}
		}

		private void miFileCloseClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.Close();
			}
		}

		private void miMapClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowMap();
			}
		}

		private void miOrganizerClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowOrganizer();
			}
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
				object obj = this.fmTimeLine;
				SysUtils.FreeAndNil(ref obj);
				this.fmTimeLine = (obj as TfmTimeLine);
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
				object obj = this.fmCalendar;
				SysUtils.FreeAndNil(ref obj);
				this.fmCalendar = (obj as TfmCalendar);
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
				object obj = this.fmNamesBook;
				SysUtils.FreeAndNil(ref obj);
				this.fmNamesBook = (obj as TfmNamesBook);
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
				object obj = this.fmCalcWidget;
				SysUtils.FreeAndNil(ref obj);
				this.fmCalcWidget = (obj as TfmCalcWidget);
			}
		}

		private void miAboutClick(object sender, EventArgs e)
		{
			TfmAbout.ShowAbout("GEDKeeper", SysUtils.GetFileVersion());
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

		private void miExportToExcelAppClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ExportToExcel(true);
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
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					TGlobalOptions.TBaseWin lb = this.FOptions.GetLastBase(i);
					if (File.Exists(lb.FileName))
					{
						TfmBase @base = this.CreateBase(lb.FileName);
						@base.Left = lb.WinRect.Left;
						@base.Top = lb.WinRect.Top;
						@base.Width = lb.WinRect.Right;
						@base.Height = lb.WinRect.Bottom;
						@base.WindowState = lb.WinState;
					}
					i++;
				}
				while (i != num);
			}
		}

		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (object.Equals(e.Button, this.tbFileNew))
			{
				this.miFileNewClick(null, null);
			}
			if (object.Equals(e.Button, this.tbFileLoad))
			{
				this.miFileLoadClick(null, null);
			}
			if (object.Equals(e.Button, this.tbFileSave))
			{
				this.miFileSaveClick(null, null);
			}
			if (object.Equals(e.Button, this.tbRecordAdd))
			{
				this.miRecordAddClick(null, null);
			}
			if (object.Equals(e.Button, this.tbRecordEdit))
			{
				this.miRecordEditClick(null, null);
			}
			if (object.Equals(e.Button, this.tbRecordDelete))
			{
				this.miRecordDeleteClick(null, null);
			}
			if (object.Equals(e.Button, this.tbUndo))
			{
				this.miUndoClick(null, null);
			}
			if (object.Equals(e.Button, this.tbRedo))
			{
				this.miRedoClick(null, null);
			}
			if (object.Equals(e.Button, this.tbFilter))
			{
				this.miFilterClick(null, null);
			}
			if (object.Equals(e.Button, this.tbTreeAncestors))
			{
				this.miTreeAncestorsClick(null, null);
			}
			if (object.Equals(e.Button, this.tbTreeDescendants))
			{
				this.miTreeDescendantsClick(null, null);
			}
			if (object.Equals(e.Button, this.tbTreeBoth))
			{
				this.miTreeBothClick(null, null);
			}
			if (object.Equals(e.Button, this.tbStats))
			{
				this.miStatsClick(null, null);
			}
			if (object.Equals(e.Button, this.tbPrev))
			{
				this.tbPrevClick(null, null);
			}
			if (object.Equals(e.Button, this.tbNext))
			{
				this.tbNextClick(null, null);
			}
		}

		private void TfmGEDKeeper_Resize(object sender, EventArgs e)
		{
			this.StatusBar.Panels[0].Width = base.Width - 50;
		}

		private void TfmGEDKeeper_Closed(object sender, EventArgs e)
		{
			this.FOptions.MWinRect = this.CheckFormRect(this);
			this.FOptions.MWinState = base.WindowState;
			SysUtils.HtmlHelp(IntPtr.Zero, null, 18u, 0u);
			this.FNamesTable.SaveToFile(SysUtils.GetAppPath() + "GEDKeeper2.nms");
			this.FNamesTable.Dispose();
			this.FOptions.SaveToFile(SysUtils.GetAppPath() + "GEDKeeper2.ini");
			this.FOptions.Dispose();
		}

		private void TfmGEDKeeper_Closing(object sender, CancelEventArgs e)
		{
			Form[] mdiChildren = base.MdiChildren;
			for (int i = mdiChildren.Length - 1; i >= 0; i--)
			{
				if (mdiChildren[i] is TfmBase)
				{
					TfmBase @base = mdiChildren[i] as TfmBase;
					if (!@base.CheckModified())
					{
						e.Cancel = true;
						return;
					}
				}
			}

			this.FOptions.ClearLastBases();

			for (int i = mdiChildren.Length - 1; i >= 0; i--)
			{
				if (mdiChildren[i] is TfmBase)
				{
					TfmBase @base = mdiChildren[i] as TfmBase;
					TGlobalOptions.TBaseWin lb = this.FOptions.AddLastBase();
					lb.FileName = @base.FileName;
					lb.WinRect = this.CheckFormRect(@base);
					lb.WinState = @base.WindowState;
					TObjectHelper.Free(@base);
				}
				else
				{
					TObjectHelper.Free(mdiChildren[i]);
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
					SysUtils.EnableWindow((uint)this.fmCalcWidget.Handle.ToInt32(), (LongBool)(-1));
				}
			}
			else
			{
				int arg_4F_0 = m.Msg;
			}
		}

		public TfmBase GetCurrentFile()
		{
			TfmBase Result = ((base.ActiveMdiChild is TfmBase) ? (base.ActiveMdiChild as TfmBase) : null);
			return Result;
		}

		public string GetCurrentFileName()
		{
			TfmBase cb = this.GetCurrentFile();
			string result = ((cb == null) ? "" : cb.FileName);
			return result;
		}

		public void AddMRU([In] string aFileName)
		{
			int idx = this.FOptions.MRUFiles.IndexOf(aFileName);
			if (idx < 0)
			{
				this.FOptions.MRUFiles.Insert(0, aFileName);
			}
			else
			{
				if (idx > 0)
				{
					this.FOptions.MRUFiles.Delete(idx);
					this.FOptions.MRUFiles.Insert(0, aFileName);
				}
			}
			this.UpdateMRU();
		}

		public TfmBase CreateBase([In] string aFileName)
		{
			TfmBase result = new TfmBase();
			result.MdiParent = this;
			result.Show();

			if (aFileName != "" && File.Exists(aFileName)) {
				result.FileLoad(aFileName);
			} else {
				result.FileNew();
			}

			return result;
		}

		public void UpdateControls(bool ForceDeactivate)
		{
			try
			{
				TfmBase cur_base;

				if (ForceDeactivate)
				{
					cur_base = null;
				}
				else
				{
					cur_base = this.GetCurrentFile();
				}

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
				this.miExportToExcelApp.Enabled = base_en;
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
				this.miOrganizer.Enabled = base_en;
				this.miScripts.Enabled = base_en;
				this.tbPrev.Enabled = (cur_base != null && cur_base.Backman.CanBackward());
				this.tbNext.Enabled = (cur_base != null && cur_base.Backman.CanForward());
				bool test_funcs = TGenEngine.IsDevComp();
				this.miUndo.Enabled = (test_funcs && cur_base != null && cur_base.Undoman.CanUndo());
				this.tbUndo.Enabled = this.miUndo.Enabled;
				this.miRedo.Enabled = (test_funcs && cur_base != null && cur_base.Undoman.CanRedo());
				this.tbRedo.Enabled = this.miRedo.Enabled;

				if (cur_base != null)
				{
					string st = GKL.LSList[50] + ": " + cur_base.FCounts[(int)rt].Total.ToString();

					if (rt == TGEDCOMRecordType.rtIndividual)
					{
						st = string.Concat(new string[]
						{
							st, 
							", ", 
							GKL.LSList[51], 
							": ", 
							cur_base.FCounts[(int)rt].Filtered.ToString()
						});
					}
					this.StatusBar.Panels[0].Text = st;
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
			string fns = SysUtils.GetAppPath() + "GEDKeeper.chm" + aTopic;
			SysUtils.HtmlHelp(this.Handle, fns, 0u, 0u);
		}

		void ILocalization.SetLang()
		{
			this.miFile.Text = GKL.LSList[0];
			this.miEdit.Text = GKL.LSList[1];
			this.miPedigree.Text = GKL.LSList[2];
			this.miService.Text = GKL.LSList[3];
			this.miWindow.Text = GKL.LSList[4];
			this.miHelp.Text = GKL.LSList[5];
			this.miFileNew.Text = GKL.LSList[6];
			this.miFileLoad.Text = GKL.LSList[7];
			this.miMRUFiles.Text = GKL.LSList[8];
			this.miFileSave.Text = GKL.LSList[9];
			this.miFileClose.Text = GKL.LSList[10];
			this.miFileProperties.Text = GKL.LSList[11];
			this.miExport.Text = GKL.LSList[12];
			this.miExportToWeb.Text = GKL.LSList[13];
			this.miExportToExcelApp.Text = GKL.LSList[14];
			this.miExportToExcelFile.Text = GKL.LSList[15];
			this.miExit.Text = GKL.LSList[16];
			this.miUndo.Text = GKL.LSList[17];
			this.miRedo.Text = GKL.LSList[18];
			this.miRecordAdd.Text = GKL.LSList[19];
			this.miRecordEdit.Text = GKL.LSList[20];
			this.miRecordDelete.Text = GKL.LSList[21];
			this.miStreamInput.Text = GKL.LSList[22] + "...";
			this.miTreeAncestors.Text = GKL.LSList[23];
			this.miTreeDescendants.Text = GKL.LSList[24];
			this.miTreeBoth.Text = GKL.LSList[25];
			this.miPedigree_dAboville.Text = GKL.LSList[26];
			this.miPedigree_Konovalov.Text = GKL.LSList[27];
			this.miMap.Text = GKL.LSList[28] + "...";
			this.miStats.Text = GKL.LSList[29] + "...";
			this.miCalc.Text = GKL.LSList[30] + "...";
			this.miNamesBook.Text = GKL.LSList[31] + "...";
			this.miCalendar.Text = GKL.LSList[32] + "...";
			this.miTimeLine.Text = GKL.LSList[33] + "...";
			this.miOrganizer.Text = GKL.LSList[34] + "...";
			this.miScripts.Text = GKL.LSList[35];
			this.miTreeTools.Text = GKL.LSList[37];
			this.miFilter.Text = GKL.LSList[38] + "...";
			this.miOptions.Text = GKL.LSList[39] + "...";
			this.miWinCascade.Text = GKL.LSList[40];
			this.miWinHTile.Text = GKL.LSList[41];
			this.miWinVTile.Text = GKL.LSList[42];
			this.miWinMinimize.Text = GKL.LSList[43];
			this.miWinArrange.Text = GKL.LSList[44];
			this.miGenResources.Text = GKL.LSList[45];
			this.miKinshipTerms.Text = GKL.LSList[46];
			this.miFAQ.Text = GKL.LSList[47];
			this.miContext.Text = GKL.LSList[48];
			this.miAbout.Text = GKL.LSList[49] + "...";
		}

		public void LoadLanguage(int LangCode)
		{
			Screen scr = Screen.PrimaryScreen;
			int i;
			if (LangCode != 1049)
			{
				bool loaded = false;

				int num = this.FOptions.LangsCount - 1;
				i = 0;
				if (num >= i)
				{
					num++;
					while ((int)this.FOptions.GetLang(i).Code != LangCode)
					{
						i++;
						if (i == num)
						{
							goto IL_66;
						}
					}
					loaded = TLangMan.LoadFromFile(this.FOptions.GetLang(i).FileName);
				}
				IL_66:
				if (!loaded)
				{
					LangCode = 1049;
				}
			}
			if (LangCode == 1049)
			{
				TLangMan.DefInit();
			}

			int num2 = base.MdiChildren.Length - 1;
			for (i = 0; i <= num2; i++)
			{
				if (base.MdiChildren[i] is ILocalization)
				{
					(base.MdiChildren[i] as ILocalization).SetLang();
				}
			}
			this.FOptions.InterfaceLang = (ushort)LangCode;

			(this as ILocalization).SetLang();
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
