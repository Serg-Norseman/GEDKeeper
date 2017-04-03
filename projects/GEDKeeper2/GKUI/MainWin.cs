/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Security.Permissions;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Export;
using GKCore.Geocoding;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.SingleInstance;
using GKCore.Types;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Dialogs;
using GKUI.Engine;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class MainWin : Form, IHost, ISingleInstanceEnforcer
    {
        private delegate void OnMessageReceivedInvoker(MessageEventArgs e);

        private class WidgetInfo
        {
            public IWidget Widget;
            public ToolStripMenuItem MenuItem;
        }

        private NamesTable fNamesTable;
        private readonly GlobalOptions fOptions;
        private List<IPlugin> fPlugins;
        private readonly Timer fAutosaveTimer;

        private readonly List<WidgetInfo> fActiveWidgets;
        private string[] fCommandArgs;

        private int fLoadingCount;
        private readonly StringList fTips;
        private PathReplacer fPathReplacer;

        private static MainWin fInstance = null;
        private static GKResourceManager fResourceManager;

        public static MainWin Instance
        {
            get { return fInstance; }
        }

        public static GKResourceManager ResourceManager
        {
            get { return fResourceManager; }
        }

        public IGeocoder Geocoder
        {
            get { return GKUtils.CreateGeocoder(fOptions); }
        }

        public INamesTable NamesTable
        {
            get { return fNamesTable; }
        }

        public GlobalOptions Options
        {
            get { return fOptions; }
        }

        public List<IPlugin> Plugins
        {
            get { return fPlugins; }
        }

        public PathReplacer PathReplacer
        {
            get { return fPathReplacer; }
        }

        #region Instance control

        public MainWin()
        {
            InitializeComponent();

            fInstance = this;
            fResourceManager = new GKResourceManager("GKResources", typeof(MainWin).Assembly);
            fOptions = GlobalOptions.Instance;

            tbFileNew.Image = GKResources.iCreateNew;
            tbFileLoad.Image = GKResources.iLoad;
            tbFileSave.Image = GKResources.iSave;
            tbRecordAdd.Image = GKResources.iRecNew;
            tbRecordEdit.Image = GKResources.iRecEdit;
            tbRecordDelete.Image = GKResources.iRecDelete;
            tbFilter.Image = GKResources.iFilter;
            tbTreeAncestors.Image = GKResources.iTreeAncestry;
            tbTreeDescendants.Image = GKResources.iTreeDescendants;
            tbTreeBoth.Image = GKResources.iTreeBoth;
            tbPedigree.Image = GKResources.iScroll;
            tbStats.Image = GKResources.iTable;
            tbPrev.Image = GKResources.iLeft1;
            tbNext.Image = GKResources.iRight1;
            tbDocPreview.Image = GKResources.iPreview;
            tbDocPrint.Image = GKResources.iPrint;

            Logger.LogInit(GKUtils.GetLogFilename());

            fActiveWidgets = new List<WidgetInfo>();

            fAutosaveTimer = new Timer(components);
            fAutosaveTimer.Stop();
            fAutosaveTimer.Enabled = false;
            fAutosaveTimer.Interval = 10 * 60 * 1000;
            fAutosaveTimer.Tick += AutosaveTimer_Tick;

            fTips = new StringList();

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

        private void AutosaveTimer_Tick(object sender, EventArgs e)
        {
            try
            {
                int num = MdiChildren.Length;
                for (int i = 0; i < num; i++) {
                    Form child = MdiChildren[i];

                    if (child is IBaseWindow) {
                        IBaseWindow baseWin = (IBaseWindow) child;

                        // file is modified, isn't updated now, and isn't now created (exists)
                        if (baseWin.Modified && !baseWin.Context.IsUpdated() && !baseWin.IsUnknown()) {
                            // TODO: if file is new and not exists - don't save it, but hint to user
                            baseWin.FileSave(baseWin.Tree.FileName);
                        }
                    }
                }
            } catch (Exception ex) {
                LogWrite("MainWin.AutosaveTimer_Tick(): " + ex.Message);
            }
        }

        private void ApplyOptions()
        {
            fAutosaveTimer.Interval = fOptions.AutosaveInterval /* min */ * 60 * 1000;
            fAutosaveTimer.Enabled = fOptions.Autosave;
        }

        #endregion

        #region Event handlers

        public void SetArgs(string[] args)
        {
            if (args == null) return;

            fCommandArgs = (string[])args.Clone();
        }

        private void LoadArgs()
        {
            if (fCommandArgs != null && fCommandArgs.Length > 0) {
                CreateBase(fCommandArgs[0]);
            }
        }

        /// <summary>
        /// Reload at startup recent opened files.
        /// </summary>
        private void ReloadRecentBases()
        {
            if (!GlobalOptions.Instance.LoadRecentFiles) return;

            try {
                BeginLoading();

                int num = fOptions.GetLastBasesCount();
                for (int i = 0; i < num; i++) {
                    string lb = fOptions.GetLastBase(i);
                    if (File.Exists(lb)) {
                        CreateBase(lb);
                    }
                }
            } finally {
                EndLoading();
            }
        }

        private void Form_Show(object sender, EventArgs e)
        {
            try
            {
                try
                {
                    BeginLoading();

                    ReloadRecentBases();

                    ProcessHolidays();
                } finally {
                    EndLoading();
                }

                UpdateMan.CheckUpdate();
            } catch (Exception ex) {
                LogWrite("MainWin.Form_Show(): " + ex.Message);
            }
        }

        private void Form_Load(object sender, EventArgs e)
        {
            try
            {
                fOptions.LoadFromFile(GetAppDataPath() + "GEDKeeper2.ini");
                fOptions.FindLanguages();
                ApplyOptions();

                RestoreWindowState();

                fNamesTable = new NamesTable();
                fNamesTable.LoadFromFile(GetAppDataPath() + "GEDKeeper2.nms");

                fPlugins = new List<IPlugin>();
                LoadPlugins(GKUtils.GetPluginsPath());
                UpdatePluginsItems();

                fPathReplacer = new PathReplacer();
                fPathReplacer.Load(GKUtils.GetAppPath() + "crossplatform.yaml");

                LoadLanguage(fOptions.InterfaceLang);

                UpdateMRU();
                UpdateControls(false);

                LoadArgs();
            } catch (Exception ex) {
                LogWrite("MainWin.Form_Load(): " + ex.Message);
            }
        }

        private void Form_Closed(object sender, FormClosedEventArgs e)
        {
            try {
                UnloadPlugins();

                fOptions.MWinRect = AppHub.UIHelper.GetFormRect(this);
                fOptions.MWinState = WindowState;

                fNamesTable.SaveToFile(GetAppDataPath() + "GEDKeeper2.nms");
                fNamesTable.Dispose();

                fOptions.SaveToFile(GetAppDataPath() + "GEDKeeper2.ini");
                fOptions.Dispose();
            } catch (Exception ex) {
                LogWrite("MainWin.Form_Closed(): " + ex.Message);
            }
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.F12) {
                #if __MonoCS__
                ShowWarning(@"This function is not supported in Linux");
                #else
                IBaseWindow curBase = GetCurrentFile();
                if (curBase == null) return;

                using (TreesAlbumExporter fb = new TreesAlbumExporter(curBase)) {
                    fb.Generate(true);
                }
                #endif
            }
        }

        private void Form_Resize(object sender, EventArgs e)
        {
            StatusBar.Panels[0].Width = Width - 50;
        }

        private void Form_Closing(object sender, FormClosingEventArgs e)
        {
            fOptions.ClearLastBases();
            for (int i = MdiChildren.Length - 1; i >= 0; i--) {
                Form mdiChild = MdiChildren[i];
                if (mdiChild is IBaseWindow) {
                    fOptions.AddLastBase((mdiChild as IBaseWindow).Tree.FileName);
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
                try {
                    BeginLoading();

                    Array a = e.Data.GetData(DataFormats.FileDrop) as Array;
                    if (a == null) return;

                    for (int i = 0; i < a.Length; i++) {
                        string fn = a.GetValue(i).ToString();
                        CreateBase(fn);
                    }
                } finally {
                    EndLoading();
                }
            } catch (Exception ex) {
                LogWrite("MainWin.Form_DragDrop(): " + ex.Message);
            }
        }

        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        protected override void WndProc(ref Message m)
        {
            base.WndProc(ref m);

            if (m.Msg == NativeMethods.WM_KEEPMODELESS) {
                foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                    widgetInfo.Widget.WidgetEnable();
                }
            }
        }

        private void StatusBar_DrawItem(object sender, StatusBarDrawItemEventArgs sbdevent)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            Bitmap pic = null;
            switch (curBase.ShieldState)
            {
                case ShieldState.None:
                    pic = (Bitmap)GKResources.iRGShieldNone.Clone();
                    break;
                case ShieldState.Middle:
                    pic = (Bitmap)GKResources.iRGShieldMid.Clone();
                    break;
                case ShieldState.Maximum:
                    pic = (Bitmap)GKResources.iRGShieldMax.Clone();
                    break;
            }

            if (pic == null) return;
            pic.MakeTransparent(pic.GetPixel(0, 0));
            sbdevent.Graphics.DrawImage(pic, sbdevent.Bounds.Left, sbdevent.Bounds.Top);
        }

        private void StatusBar_PanelClick(object sender, StatusBarPanelClickEventArgs e)
        {
            if (e.StatusBarPanel == StatusBarPanel2 && e.Clicks == 2) {
                IBaseWindow curBase = GetCurrentFile();
                if (curBase == null) return;

                ShieldState ss = curBase.ShieldState;
                if (ss == ShieldState.None) {
                    ss = ShieldState.Maximum;
                } else {
                    ss = (ShieldState)((int)ss + 1);
                }

                curBase.ShieldState = ss;
                StatusBar.Invalidate();
            }
        }

        private void ToolBar1_ButtonClick(object sender, EventArgs e)
        {
            if (sender == tbFileNew) {
                miFileNew_Click(null, null);
            } else if (sender == tbFileLoad) {
                miFileLoad_Click(null, null);
            } else if (sender == tbFileSave) {
                miFileSave_Click(null, null);
            } else if (sender == tbRecordAdd) {
                miRecordAdd_Click(null, null);
            } else if (sender == tbRecordEdit) {
                miRecordEdit_Click(null, null);
            } else if (sender == tbRecordDelete) {
                miRecordDelete_Click(null, null);
            } else if (sender == tbFilter) {
                miFilter_Click(null, null);
            } else if (sender == tbTreeAncestors) {
                miTreeAncestors_Click(null, null);
            } else if (sender == tbTreeDescendants) {
                miTreeDescendants_Click(null, null);
            } else if (sender == tbTreeBoth) {
                miTreeBoth_Click(null, null);
            } else if (sender == tbStats) {
                miStats_Click(null, null);
            } else if (sender == tbPrev) {
                tbPrev_Click(null, null);
            } else if (sender == tbNext) {
                tbNext_Click(null, null);
            } else if (sender == tbDocPrint) {
                tbDocPrint_Click(null, null);
            } else if (sender == tbDocPreview) {
                tbDocPreview_Click(null, null);
            }
        }

        private void miExit_Click(object sender, EventArgs e)
        {
            Close();
        }

        #endregion

        #region Misc functions

        private void RestoreWindowState()
        {
            try
            {
                ExtRect mwinRect = fOptions.MWinRect;
                if (!mwinRect.IsEmpty()) {
                    Left = mwinRect.Left;
                    Top = mwinRect.Top;
                    Width = mwinRect.Right - mwinRect.Left + 1;
                    Height = mwinRect.Bottom - mwinRect.Top + 1;
                } else {
                    //--------------------------------------------------------------
                    // 2016-09-30 Ruslan Garipov <brigadir15@gmail.com>
                    // FIXME: This is an ERROR. You can't use explicit numbers like
                    // 600 or 800 to specify size of a window. You must take into
                    // account system DPI (starting from Windows 8.1: a monitor's
                    // DPI) to convert logical size to physical. At least you have
                    // to do this in native win32 world.
                    //--------------------------------------------------------------
                    Left = (Screen.PrimaryScreen.WorkingArea.Width - 800) / 2;
                    Top = (Screen.PrimaryScreen.WorkingArea.Height - 600) / 2;
                    Width = 800;
                    Height = 600;
                }
                WindowState = fOptions.MWinState;
            }
            catch (Exception ex)
            {
                LogWrite("MainWin.RestoreWindowState(): " + ex.Message);
            }
        }

        private string GetLanguageSign()
        {
            string lngSign;

            LangRecord lngrec = fOptions.GetLangByCode(fOptions.InterfaceLang);
            if (lngrec == null) {
                /*if (fOptions.InterfaceLang == LangMan.LS_DEF_CODE) {
                    lngSign = LangMan.LS_DEF_SIGN;
                } else {
                    lngSign = string.Empty;
                }*/
                lngSign = LangMan.LS_DEF_SIGN;
            } else {
                lngSign = lngrec.Sign;
            }

            return lngSign;
        }

        private static ushort RequestLanguage()
        {
            using (LanguageSelectDlg dlg = new LanguageSelectDlg())
            {
                if (dlg.ShowDialog() == DialogResult.OK)
                {
                    return (ushort)dlg.SelectedLanguage;
                }
            }

            return LangMan.LS_DEF_CODE;
        }

        public void LoadLanguage(int langCode)
        {
            try
            {
                if (langCode <= 0) {
                    langCode = RequestLanguage();
                }

                if (langCode != LangMan.LS_DEF_CODE) {
                    bool loaded = false;

                    foreach (LangRecord langRec in fOptions.Languages) {
                        if (langRec.Code == langCode) {
                            loaded = LangMan.LoadFromFile(langRec.FileName);
                            break;
                        }
                    }

                    if (!loaded) langCode = LangMan.LS_DEF_CODE;
                }

                if (langCode == LangMan.LS_DEF_CODE) {
                    LangMan.DefInit();
                }

                foreach (Form child in MdiChildren) {
                    ILocalization localChild = (child as ILocalization);

                    if (localChild != null) {
                        localChild.SetLang();
                    }
                }

                SetLang();

                fOptions.InterfaceLang = (ushort)langCode;

                UpdatePluginsLanguage();
            }
            catch (Exception ex)
            {
                LogWrite("MainWin.LoadLanguage(): " + ex.Message);
            }
        }

        private void ProcessHolidays()
        {
            try
            {
                if (!fOptions.ShowTips) return;

                Holidays holidays = new Holidays();

                // TODO: We need a reference to the country, not the language
                string lngSign = GetLanguageSign();
                if (!string.IsNullOrEmpty(lngSign)) {
                    holidays.Load(GKUtils.GetLangsPath() + "holidays_" + lngSign + ".yaml");
                }

                holidays.CollectTips(fTips);
            }
            catch (Exception ex)
            {
                LogWrite("MainWin.ProcessHolidays(): " + ex.Message);
            }
        }

        public DialogResult ShowModalEx(Form form, bool keepModeless)
        {
            if (form == null) return DialogResult.None;

            if (keepModeless) {
                #if !__MonoCS__
                NativeMethods.PostMessage(Handle, NativeMethods.WM_KEEPMODELESS, IntPtr.Zero, IntPtr.Zero);
                #endif
            }

            return form.ShowDialog();
        }

        public void RequestGeoCoords(string searchValue, IList<GeoPoint> pointsList)
        {
            if (string.IsNullOrEmpty(searchValue))
                throw new ArgumentNullException("searchValue");

            if (pointsList == null)
                throw new ArgumentNullException("pointsList");

            try
            {
                IEnumerable<GeoPoint> geoPoints = Geocoder.Geocode(searchValue, 1);
                foreach (GeoPoint pt in geoPoints)
                {
                    pointsList.Add(pt);
                }
            } catch (Exception ex) {
                LogWrite("MainWin.RequestGeoCoords(): " + ex.Message);
            }
        }

        #endregion

        #region MRU functions

        private void MRUFileClick(object sender, EventArgs e)
        {
            int idx = (int)((GKToolStripMenuItem)sender).Tag;
            CreateBase(fOptions.MRUFiles[idx].FileName);
        }

        private void UpdateMRU()
        {
            try {
                miMRUFiles.Enabled = (fOptions.MRUFiles.Count > 0);
                miMRUFiles.DropDownItems.Clear();
                MenuMRU.Items.Clear();

                int num = fOptions.MRUFiles.Count;
                for (int i = 0; i < num; i++) {
                    string fn = fOptions.MRUFiles[i].FileName;

                    GKToolStripMenuItem mi = new GKToolStripMenuItem(fn, i);
                    mi.Click += MRUFileClick;
                    miMRUFiles.DropDownItems.Add(mi);

                    GKToolStripMenuItem tsmi = new GKToolStripMenuItem(fn, i);
                    tsmi.Click += MRUFileClick;
                    MenuMRU.Items.Add(tsmi);
                }
            } catch (Exception ex) {
                LogWrite("MainWin.UpdateMRU(): " + ex.Message);
            }
        }

        public void AddMRU(string fileName)
        {
            int idx = fOptions.MRUFiles_IndexOf(fileName);

            MRUFile mf;
            if (idx >= 0) {
                mf = fOptions.MRUFiles[idx];
                fOptions.MRUFiles.RemoveAt(idx);
            } else {
                mf = new MRUFile(fileName);
            }

            fOptions.MRUFiles.Insert(0, mf);

            UpdateMRU();
        }

        public void CheckMRUWin(string fileName, Form frm)
        {
            int idx = fOptions.MRUFiles_IndexOf(fileName);
            if (idx < 0) return;

            MRUFile mf = fOptions.MRUFiles[idx];
            mf.WinRect = AppHub.UIHelper.GetFormRect(frm);
            mf.WinState = frm.WindowState;
        }

        public void RestoreMRU(IBaseWindow baseWin, string fileName)
        {
            int idx = fOptions.MRUFiles_IndexOf(fileName);
            if (idx < 0) return;

            MRUFile mf = fOptions.MRUFiles[idx];
            AppHub.UIHelper.RestoreFormRect(baseWin as Form, mf.WinRect, mf.WinState);
        }

        #endregion

        #region Base Management

        public void UpdateNavControls()
        {
            try
            {
                IWorkWindow workWin = GetWorkWindow();

                tbPrev.Enabled = (workWin != null && workWin.NavCanBackward());
                tbNext.Enabled = (workWin != null && workWin.NavCanForward());
            } catch (Exception ex) {
                LogWrite("MainWin.UpdateNavControls(): " + ex.Message);
            }
        }

        public void UpdateControls(bool forceDeactivate)
        {
            try
            {
                IBaseWindow curBase = ((forceDeactivate) ? null : GetCurrentFile());
                IChartWindow curChart = ((ActiveMdiChild is IChartWindow) ? ((IChartWindow) ActiveMdiChild) : null);

                IWorkWindow workWin = GetWorkWindow();

                GEDCOMRecordType rt = (curBase == null) ? GEDCOMRecordType.rtNone : curBase.GetSelectedRecordType();
                bool baseEn = (rt != GEDCOMRecordType.rtNone);

                miFileSave.Enabled = baseEn || (curChart != null);
                miFileSaveAs.Enabled = miFileSave.Enabled;
                tbFileSave.Enabled = miFileSave.Enabled;

                miRecordAdd.Enabled = baseEn;
                tbRecordAdd.Enabled = miRecordAdd.Enabled;
                miRecordEdit.Enabled = baseEn;
                tbRecordEdit.Enabled = miRecordEdit.Enabled;
                miRecordDelete.Enabled = baseEn;
                tbRecordDelete.Enabled = miRecordDelete.Enabled;
                miStats.Enabled = baseEn;
                tbStats.Enabled = miStats.Enabled;

                miFilter.Enabled = (workWin != null && workWin.AllowFilter());
                tbFilter.Enabled = miFilter.Enabled;

                miSearch.Enabled = (workWin != null && workWin.AllowQuickSearch());

                tbDocPrint.Enabled = (curChart != null && curChart.AllowPrint());
                tbDocPreview.Enabled = (curChart != null && curChart.AllowPrint());

                miTreeTools.Enabled = baseEn;
                miExportToFamilyBook.Enabled = baseEn;
                miExportToExcelFile.Enabled = baseEn;
                miFileClose.Enabled = baseEn;
                miFileProperties.Enabled = baseEn;
                miOrganizer.Enabled = baseEn;
                miSlideshow.Enabled = baseEn;
                miScripts.Enabled = baseEn;

                bool indivEn = baseEn && rt == GEDCOMRecordType.rtIndividual;

                miTreeAncestors.Enabled = indivEn;
                tbTreeAncestors.Enabled = miTreeAncestors.Enabled;
                miTreeDescendants.Enabled = indivEn;
                tbTreeDescendants.Enabled = miTreeDescendants.Enabled;
                miTreeBoth.Enabled = indivEn;
                tbTreeBoth.Enabled = miTreeBoth.Enabled;
                miPedigree.Enabled = indivEn;
                tbPedigree.Enabled = miPedigree.Enabled;
                miPedigree_dAboville.Enabled = indivEn;
                miPedigree_Konovalov.Enabled = indivEn;

                if (workWin != null) {
                    StatusBar.Panels[0].Text = workWin.GetStatusString();
                }

                UpdateNavControls();

                StatusBar.Invalidate();
            } catch (Exception ex) {
                LogWrite("MainWin.UpdateControls(): " + ex.Message);
            }
        }

        public string GetCurrentFileName()
        {
            IBaseWindow cb = GetCurrentFile();
            string result = ((cb == null) ? "" : cb.Tree.FileName);
            return result;
        }

        public IBaseWindow FindBase(string fileName)
        {
            IBaseWindow result = null;

            int num = MdiChildren.Length;
            for (int i = 0; i < num; i++) {
                Form child = MdiChildren[i];

                IBaseWindow baseWin = child as IBaseWindow;
                if (baseWin == null) continue;

                if (string.Equals(baseWin.Tree.FileName, fileName)) {
                    result = baseWin;
                    break;
                }
            }

            return result;
        }

        public void ShowTips()
        {
            if (fTips.Count <= 0) return;

            fOptions.ShowTips =
                DayTipsDlg.ShowTipsEx(LangMan.LS(LSID.LSID_BirthDays),
                                      fOptions.ShowTips, fTips, Handle);

            fTips.Clear();
        }

        private void BeginLoading()
        {
            fLoadingCount++;
        }

        private void EndLoading()
        {
            fLoadingCount--;

            if (fLoadingCount == 0)
            {
                ShowTips();
            }
        }

        public IBaseWindow CreateBase(string fileName)
        {
            IBaseWindow result = null;

            try {
                try {
                    BeginLoading();

                    result = FindBase(fileName);
                    if (result != null) {
                        result.Activate();
                        return result;
                    }

                    result = new BaseWin();
                    ShowMDI((Form) result);

                    if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName)) {
                        result.FileLoad(fileName);
                        result.CollectTips(fTips);
                    } else {
                        result.FileNew();
                    }

                    RestoreMRU(result, fileName);
                } finally {
                    EndLoading();
                }
            } catch (Exception ex) {
                LogWrite("MainWin.CreateBase(): " + ex.Message);
            }

            return null;
        }

        public void CriticalSave()
        {
            try
            {
                int num = MdiChildren.Length;
                for (int i = 0; i < num; i++) {
                    IBaseWindow baseWin = MdiChildren[i] as IBaseWindow;
                    if (baseWin != null) {
                        baseWin.CriticalSave();
                    }
                }
            } catch (Exception ex) {
                LogWrite("MainWin.CriticalSave(): " + ex.Message);
            }
        }

        private void miUndo_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            curBase.Context.DoUndo();
        }

        private void miRedo_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            curBase.Context.DoRedo();
        }

        private void miExportToFamilyBook_Click(object sender, EventArgs e)
        {
            #if __MonoCS__
            ShowWarning(@"This function is not supported in Linux");
            #else
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            using (FamilyBookExporter fb = new FamilyBookExporter(curBase)) {
                fb.Generate(true);
            }
            #endif
        }

        private void miExportToExcelFile_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            using (ExcelExporter exExp = new ExcelExporter(curBase)) {
                exExp.Options = fOptions;
                exExp.Generate(true);
            }
        }

        private void miFileProperties_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (FilePropertiesDlg dlgFileProps = new FilePropertiesDlg(curBase)) {
                    ShowModalEx(dlgFileProps, false);
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miScripts_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (ScriptEditWin dmn = new ScriptEditWin(curBase)) {
                    ShowModalEx(dmn, false);
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miTreeTools_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (TreeToolsWin fmTreeTools = new TreeToolsWin(curBase)) {
                    ShowModalEx(fmTreeTools, false);
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miOptions_Click(object sender, EventArgs e)
        {
            using (OptionsDlg dlgOptions = new OptionsDlg(this))
            {
                Form activeForm = ActiveMdiChild;
                if (activeForm is IBaseWindow) dlgOptions.SetPage(OptionsPage.opInterface);
                if (activeForm is IChartWindow) {
                    if (activeForm is CircleChartWin) {
                        dlgOptions.SetPage(OptionsPage.opAncestorsCircle);
                    } else {
                        dlgOptions.SetPage(OptionsPage.opTreeChart);
                    }
                }

                if (dlgOptions.ShowDialog() == DialogResult.OK)
                {
                    ApplyOptions();

                    foreach (Form child in MdiChildren)
                    {
                        if (child is IWorkWindow) {
                            (child as IWorkWindow).UpdateView();
                        }
                    }
                }
            }
        }

        private void miFileClose_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            curBase.Close();
        }

        private void miFileNew_Click(object sender, EventArgs e)
        {
            CreateBase("");
        }

        public string GetUserFilesPath(string filePath)
        {
            string ufPath = filePath;
            if (Directory.Exists(ufPath)) return ufPath;

            ufPath = fOptions.LastDir;
            if (Directory.Exists(ufPath)) return ufPath;

            ufPath = GKUtils.GetHomePath();
            return ufPath;
        }

        private void miFileLoad_Click(object sender, EventArgs e)
        {
            string homePath = GetUserFilesPath("");

            string fileName = AppHub.StdDialogs.GetOpenFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName)) {
                CreateBase(fileName);
            }
        }

        private void miFileSaveAs_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile(true);
            if (curBase == null) return;

            string homePath = GetUserFilesPath(Path.GetDirectoryName(curBase.Tree.FileName));

            string fileName = AppHub.StdDialogs.GetSaveFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT, curBase.Tree.FileName, false);
            if (!string.IsNullOrEmpty(fileName)) {
                curBase.FileSave(fileName);
            }
        }

        public void miFileSave_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile(true);
            if (curBase == null) return;

            if (!curBase.IsUnknown()) {
                curBase.FileSave(curBase.Tree.FileName);
            } else {
                miFileSaveAs_Click(sender, e);
            }
        }

        private void miRecordAdd_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            curBase.RecordAdd();
        }

        private void miRecordEdit_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            curBase.RecordEdit(sender, e);
        }

        private void miRecordDelete_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            curBase.RecordDelete();
        }

        private void miSearch_Click(object sender, EventArgs e)
        {
            IWorkWindow win = GetWorkWindow();
            if (win == null) return;

            win.QuickSearch();
        }

        private void miFilter_Click(object sender, EventArgs e)
        {
            IWorkWindow win = GetWorkWindow();
            if (win == null) return;

            win.SetFilter();
        }

        private void tbPrev_Click(object sender, EventArgs e)
        {
            IWorkWindow win = GetWorkWindow();
            if (win == null) return;

            win.NavPrev();
        }

        private void tbNext_Click(object sender, EventArgs e)
        {
            IWorkWindow win = GetWorkWindow();
            if (win == null) return;

            win.NavNext();
        }

        private void tbDocPrint_Click(object sender, EventArgs e)
        {
            IChartWindow chartWin = GetWorkWindow() as IChartWindow;
            if (chartWin != null && chartWin.AllowPrint()) {
                chartWin.DoPrint();
            }
        }

        private void tbDocPreview_Click(object sender, EventArgs e)
        {
            IChartWindow chartWin = GetWorkWindow() as IChartWindow;
            if (chartWin != null && chartWin.AllowPrint()) {
                chartWin.DoPrintPreview();
            }
        }

        private void miMap_Click(object sender, EventArgs e)
        {
            #if __MonoCS__
            ShowWarning(@"This function is not supported in Linux");
            #else
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;
            
            MapsViewerWin mapsWin = new MapsViewerWin(curBase);
            mapsWin.MdiParent = this;
            mapsWin.ProcessMap();
            #endif
        }

        private void miOrganizer_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            using (OrganizerWin dlg = new OrganizerWin(curBase)) {
                ShowModalEx(dlg, false);
            }
        }

        private void miRelationshipCalculator_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            using (RelationshipCalculatorDlg relCalc = new RelationshipCalculatorDlg(curBase)) {
                relCalc.ShowDialog();
            }
        }

        private void miSlideshow_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            SlideshowWin win = new SlideshowWin(curBase);
            ShowMDI(win);
        }

        private void miStats_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            List<GEDCOMRecord> selectedRecords = curBase.GetContentList(GEDCOMRecordType.rtIndividual);

            StatisticsWin win = new StatisticsWin(curBase, selectedRecords);
            ShowMDI(win);
        }

        private void GeneratePedigree(PedigreeExporter.PedigreeKind kind)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            using (PedigreeExporter p = new PedigreeExporter(curBase)) {
                p.Root = curBase.GetSelectedPerson();
                p.Options = fOptions;
                p.ShieldState = curBase.ShieldState;
                p.Kind = kind;
                p.Generate(true);
            }
        }

        private void miPedigreeAscend_Click(object sender, EventArgs e)
        {
            GeneratePedigree(PedigreeExporter.PedigreeKind.pkAscend);
        }

        private void miPedigree_dAbovilleClick(object sender, EventArgs e)
        {
            GeneratePedigree(PedigreeExporter.PedigreeKind.pkDescend_dAboville);
        }

        private void miPedigree_KonovalovClick(object sender, EventArgs e)
        {
            GeneratePedigree(PedigreeExporter.PedigreeKind.pkDescend_Konovalov);
        }

        private void miTreeAncestors_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            if (TreeChartWin.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartKind.ckAncestors)) {
                TreeChartWin fmChart = new TreeChartWin(curBase, curBase.GetSelectedPerson());
                fmChart.ChartKind = TreeChartKind.ckAncestors;
                fmChart.GenChart(true);
            }
        }

        private void miTreeDescendants_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            if (TreeChartWin.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartKind.ckDescendants)) {
                TreeChartWin fmChart = new TreeChartWin(curBase, curBase.GetSelectedPerson());
                fmChart.ChartKind = TreeChartKind.ckDescendants;
                fmChart.GenChart(true);
            }
        }

        private void miTreeBoth_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            if (TreeChartWin.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartKind.ckBoth)) {
                TreeChartWin fmChart = new TreeChartWin(curBase, curBase.GetSelectedPerson());
                fmChart.ChartKind = TreeChartKind.ckBoth;
                fmChart.GenChart(true);
            }
        }

        private void miAncestorsCircle_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            CircleChartWin fmChart = new CircleChartWin(curBase, curBase.GetSelectedPerson(), CircleChartType.Ancestors);
            fmChart.GenChart(true);
        }

        private void miDescendantsCircle_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = GetCurrentFile();
            if (curBase == null) return;

            CircleChartWin fmChart = new CircleChartWin(curBase, curBase.GetSelectedPerson(), CircleChartType.Descendants);
            fmChart.GenChart(true);
        }

        #endregion

        #region Help and Windows

        private void miLogSend_Click(object sender, EventArgs e)
        {
            SysUtils.SendMail(GKData.APP_MAIL, "GEDKeeper: feedback", "This automatic notification of error.", GKUtils.GetLogFilename());
        }

        private void miLogView_Click(object sender, EventArgs e)
        {
            SysUtils.LoadExtFile(GKUtils.GetLogFilename());
        }

        private void miAbout_Click(object sender, EventArgs e)
        {
            AboutDlg.ShowAbout();
        }

        public void ShowHelpTopic(string topic)
        {
            string lngSign = GetLanguageSign();
            if (string.IsNullOrEmpty(lngSign)) return;

            string helpPath = GKUtils.GetHelpPath(lngSign);

            if (string.IsNullOrEmpty(topic)) {
                topic = helpPath + "GEDKeeper2.html";
            } else {
                topic = helpPath + topic;
            }

            if (!File.Exists(topic)) {
                AppHub.StdDialogs.ShowError(@"For that language help is unavailable");
                return;
            }

            SysUtils.LoadExtFile(topic);
        }

        private void miContext_Click(object sender, EventArgs e)
        {
            ShowHelpTopic("");
        }

        private void miWinCascade_Click(object sender, EventArgs e)
        {
            LayoutMdi(MdiLayout.Cascade);
        }

        private void miWinHTile_Click(object sender, EventArgs e)
        {
            LayoutMdi(MdiLayout.TileHorizontal);
        }

        private void miWinVTile_Click(object sender, EventArgs e)
        {
            LayoutMdi(MdiLayout.TileVertical);
        }

        private void miWinMinimize_Click(object sender, EventArgs e)
        {
            for (int i = MdiChildren.Length - 1; i >= 0; i--) {
                MdiChildren[i].WindowState = FormWindowState.Minimized;
            }
        }

        private void miWinArrange_Click(object sender, EventArgs e)
        {
            LayoutMdi(MdiLayout.ArrangeIcons);
        }

        private void miWindow_DropDownOpening(object sender, EventArgs e)
        {
            Form activeChild = ActiveMdiChild;
            if (activeChild == null) return;

            // platform: in Mono here is bug, but code works without this line
            #if !__MonoCS__
            ActivateMdiChild(null);
            #endif

            ActivateMdiChild(activeChild);
        }

        #endregion

        #region ILocalization implementation

        public void SetLang()
        {
            miFile.Text = LangMan.LS(LSID.LSID_MIFile);
            miEdit.Text = LangMan.LS(LSID.LSID_MIEdit);
            miPedigree.Text = LangMan.LS(LSID.LSID_MIPedigree);
            miService.Text = LangMan.LS(LSID.LSID_MIService);
            miWindow.Text = LangMan.LS(LSID.LSID_MIWindow);
            miHelp.Text = LangMan.LS(LSID.LSID_MIHelp);

            miFileNew.Text = LangMan.LS(LSID.LSID_MIFileNew);
            miFileLoad.Text = LangMan.LS(LSID.LSID_MIFileLoad);
            miMRUFiles.Text = LangMan.LS(LSID.LSID_MIMRUFiles);
            miFileSave.Text = LangMan.LS(LSID.LSID_MIFileSave);
            miFileSaveAs.Text = LangMan.LS(LSID.LSID_MIFileSaveAs);
            miFileClose.Text = LangMan.LS(LSID.LSID_MIFileClose);
            miFileProperties.Text = LangMan.LS(LSID.LSID_MIFileProperties) + @"...";
            miExport.Text = LangMan.LS(LSID.LSID_MIExport);
            miExportToFamilyBook.Text = LangMan.LS(LSID.LSID_MIExportToFamilyBook);
            miExportToExcelFile.Text = LangMan.LS(LSID.LSID_MIExportToExcelFile);
            miExit.Text = LangMan.LS(LSID.LSID_MIExit);

            miRecordAdd.Text = LangMan.LS(LSID.LSID_MIRecordAdd);
            miRecordEdit.Text = LangMan.LS(LSID.LSID_MIRecordEdit);
            miRecordDelete.Text = LangMan.LS(LSID.LSID_MIRecordDelete);

            miTreeAncestors.Text = LangMan.LS(LSID.LSID_MITreeAncestors);
            miTreeDescendants.Text = LangMan.LS(LSID.LSID_MITreeDescendants);
            miTreeBoth.Text = LangMan.LS(LSID.LSID_MITreeBoth);
            miPedigreeAscend.Text = LangMan.LS(LSID.LSID_MIPedigreeAscend);
            miPedigree_dAboville.Text = LangMan.LS(LSID.LSID_MIPedigree_dAboville);
            miPedigree_Konovalov.Text = LangMan.LS(LSID.LSID_MIPedigree_Konovalov);

            miMap.Text = LangMan.LS(LSID.LSID_MIMap) + @"...";
            miStats.Text = LangMan.LS(LSID.LSID_MIStats) + @"...";
            miSearch.Text = LangMan.LS(LSID.LSID_Search);
            miAncestorsCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
            miDescendantsCircle.Text = LangMan.LS(LSID.LSID_DescendantsCircle);
            miRelationshipCalculator.Text = LangMan.LS(LSID.LSID_RelationshipCalculator);

            miOrganizer.Text = LangMan.LS(LSID.LSID_MIOrganizer) + @"...";
            miSlideshow.Text = LangMan.LS(LSID.LSID_Slideshow) + @"...";
            miScripts.Text = LangMan.LS(LSID.LSID_MIScripts);
            miTreeTools.Text = LangMan.LS(LSID.LSID_MITreeTools);
            miFilter.Text = LangMan.LS(LSID.LSID_MIFilter) + @"...";
            miOptions.Text = LangMan.LS(LSID.LSID_MIOptions) + @"...";

            miWinCascade.Text = LangMan.LS(LSID.LSID_MIWinCascade);
            miWinHTile.Text = LangMan.LS(LSID.LSID_MIWinHTile);
            miWinVTile.Text = LangMan.LS(LSID.LSID_MIWinVTile);
            miWinMinimize.Text = LangMan.LS(LSID.LSID_MIWinMinimize);
            miWinArrange.Text = LangMan.LS(LSID.LSID_MIWinArrange);

            miContext.Text = LangMan.LS(LSID.LSID_MIContext);
            miAbout.Text = LangMan.LS(LSID.LSID_MIAbout) + @"...";
            miLogSend.Text = LangMan.LS(LSID.LSID_LogSend);
            miLogView.Text = LangMan.LS(LSID.LSID_LogView);
            miPlugins.Text = LangMan.LS(LSID.LSID_Plugins);

            tbFileNew.ToolTipText = LangMan.LS(LSID.LSID_FileNewTip);
            tbFileLoad.ToolTipText = LangMan.LS(LSID.LSID_FileLoadTip);
            tbFileSave.ToolTipText = LangMan.LS(LSID.LSID_FileSaveTip);
            tbRecordAdd.ToolTipText = LangMan.LS(LSID.LSID_RecordAddTip);
            tbRecordEdit.ToolTipText = LangMan.LS(LSID.LSID_RecordEditTip);
            tbRecordDelete.ToolTipText = LangMan.LS(LSID.LSID_RecordDeleteTip);
            tbFilter.ToolTipText = LangMan.LS(LSID.LSID_FilterTip);
            tbTreeAncestors.ToolTipText = LangMan.LS(LSID.LSID_TreeAncestorsTip);
            tbTreeDescendants.ToolTipText = LangMan.LS(LSID.LSID_TreeDescendantsTip);
            tbTreeBoth.ToolTipText = LangMan.LS(LSID.LSID_TreeBothTip);
            tbPedigree.ToolTipText = LangMan.LS(LSID.LSID_PedigreeTip);
            miPedigree_dAboville2.Text = LangMan.LS(LSID.LSID_Pedigree_dAbovilleTip);
            miPedigree_Konovalov2.Text = LangMan.LS(LSID.LSID_Pedigree_KonovalovTip);
            tbStats.ToolTipText = LangMan.LS(LSID.LSID_StatsTip);

            tbDocPrint.ToolTipText = LangMan.LS(LSID.LSID_DocPrint);
            tbDocPreview.ToolTipText = LangMan.LS(LSID.LSID_DocPreview);

            tbPrev.ToolTipText = LangMan.LS(LSID.LSID_PrevRec);
            tbNext.ToolTipText = LangMan.LS(LSID.LSID_NextRec);
        }

        #endregion

        #region Plugins support

        private void UpdatePluginsLanguage()
        {
            if (fPlugins == null) return;

            foreach (IPlugin plugin in fPlugins) {
                plugin.OnLanguageChange();
            }

            int num = miPlugins.DropDownItems.Count;
            for (int i = 0; i < num; i++) {
                ToolStripItem mi = miPlugins.DropDownItems[i];
                IPlugin plugin = (IPlugin)mi.Tag;
                mi.Text = plugin.DisplayName;
            }
        }

        private static void Plugin_Click(object sender, EventArgs e)
        {
            ToolStripMenuItem item = sender as ToolStripMenuItem;
            if (item == null) return;

            IPlugin plugin = item.Tag as IPlugin;
            if (plugin == null) return;

            plugin.Execute();
        }

        private void UpdatePluginsItems()
        {
            try {
                miPlugins.Visible = (fPlugins.Count > 0);
                miPlugins.DropDownItems.Clear();

                fActiveWidgets.Clear();

                foreach (IPlugin plugin in fPlugins) {
                    string dispName = plugin.DisplayName;

                    ToolStripMenuItem mi = new ToolStripMenuItem(dispName/*, i*/);
                    mi.Click += Plugin_Click;
                    mi.Tag = plugin;
                    miPlugins.DropDownItems.Add(mi);

                    if (plugin is IWidget) {
                        WidgetInfo widInfo = new WidgetInfo();
                        widInfo.Widget = (plugin as IWidget);
                        widInfo.MenuItem = mi;
                        fActiveWidgets.Add(widInfo);

                        (plugin as IWidget).WidgetInit(this);
                    }
                }
            } catch (Exception ex) {
                LogWrite("MainWin.UpdatePluginsItems(): " + ex.Message);
            }
        }

        private void UnloadPlugins()
        {
            try {
                if (fPlugins == null) return;

                foreach (IPlugin plugin in fPlugins) {
                    plugin.Shutdown();
                }
            } catch (Exception ex) {
                LogWrite("MainWin.UnloadPlugins(): " + ex.Message);
            }
        }

        private void LoadPlugins(string path)
        {
            if (!Directory.Exists(path)) return;

            try {
                AppDomain.CurrentDomain.SetupInformation.PrivateBinPath = path;

                Type pluginType = typeof(IPlugin);
                string[] pluginFiles = Directory.GetFiles(path, "*.dll");

                foreach (string pfn in pluginFiles) {
                    try {
                        Assembly asm;

                        try {
                            AssemblyName assemblyName = AssemblyName.GetAssemblyName(pfn);
                            asm = Assembly.Load(assemblyName);
                        } catch {
                            asm = null;
                            // block exceptions for bad or non-dotnet assemblies
                        }

                        if (asm == null) continue;

                        Type[] types = asm.GetTypes();
                        foreach (Type type in types) {
                            if (type.IsInterface || type.IsAbstract) continue;
                            if (type.GetInterface(pluginType.FullName) == null) continue;

                            IPlugin plugin = (IPlugin)Activator.CreateInstance(type);
                            plugin.Startup(this);
                            fPlugins.Add(plugin);
                        }
                    } catch (Exception ex) {
                        LogWrite("MainWin.LoadPlugin(" + pfn + "): " + ex.Message);
                    }
                }
            } catch (Exception ex) {
                LogWrite("MainWin.LoadPlugins(" + path + "): " + ex.Message);
            }
        }

        #endregion

        #region IHost implementation

        public bool IsUnix()
        {
            return SysUtils.IsUnix();
        }

        public void ShowWarning(string msg)
        {
            AppHub.StdDialogs.ShowWarning(msg);
        }

        public ILangMan CreateLangMan(object sender)
        {
            if (sender == null)
                return null;

            //CultureInfo cultInfo = new CultureInfo(fOptions.InterfaceLang);
            //string ext = cultInfo.ThreeLetterISOLanguageName;
            string lngSign = GetLanguageSign();

            Assembly asm = sender.GetType().Assembly;
            Module[] mods = asm.GetModules();
            string asmFile = mods[0].FullyQualifiedName;

            string langFile = Path.ChangeExtension(asmFile, "." + lngSign);
            if (!File.Exists(langFile)) {
                langFile = Path.ChangeExtension(asmFile, "." + LangMan.LS_DEF_SIGN);
            }

            LangManager langMan = new LangManager();
            bool res = langMan.LoadFromFile(langFile);
            return (res) ? langMan : null;
        }

        public IBaseWindow GetCurrentFile(bool extMode = false)
        {
            IChartWindow curChart = ((ActiveMdiChild is IChartWindow) ? ((IChartWindow) ActiveMdiChild) : null);
            IBaseWindow result;

            if (extMode && curChart != null) {
                result = curChart.Base;
            } else {
                result = ((ActiveMdiChild is IBaseWindow) ? ((IBaseWindow) ActiveMdiChild) : null);
            }

            return result;
        }

        public IWorkWindow GetWorkWindow()
        {
            Form activeForm = ActiveMdiChild;
            return (activeForm is IWorkWindow) ? (IWorkWindow) activeForm : null;
        }

        public void LogWrite(string msg)
        {
            Logger.LogWrite(msg);
        }

        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            if (fPlugins == null) return;
            if (baseWin == null || record == null) return;

            foreach (IPlugin plugin in fPlugins) {
                ISubscriber subscriber = (plugin as ISubscriber);
                if (subscriber == null) continue;

                try {
                    subscriber.NotifyRecord(baseWin, record, action);
                } catch (Exception ex) {
                    Logger.LogWrite("MainWin.NotifyRecord(): " + ex.Message);
                }
            }
        }

        public string GetAppDataPath()
        {
            return GKUtils.GetAppDataPath();
        }

        private WidgetInfo FindWidgetInfo(IWidget widget)
        {
            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                if (widgetInfo.Widget == widget) {
                    return widgetInfo;
                }
            }

            return null;
        }

        public void WidgetShow(IWidget widget)
        {
            WidgetInfo widInfo = FindWidgetInfo(widget);
            if (widInfo == null) return;

            if (widInfo.MenuItem != null) widInfo.MenuItem.Checked = true;
        }

        public void WidgetClose(IWidget widget)
        {
            WidgetInfo widInfo = FindWidgetInfo(widget);
            if (widInfo == null) return;

            if (widInfo.MenuItem != null) widInfo.MenuItem.Checked = false;
        }

        public bool IsWidgetActive(IWidget widget)
        {
            WidgetInfo widInfo = FindWidgetInfo(widget);
            if (widInfo == null || widInfo.MenuItem == null) {
                return false;
            } else {
                return widInfo.MenuItem.Checked;
            }
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.BaseChanged(baseWin);
            }
        }

        public void BaseClosed(IBaseWindow baseWin)
        {
            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.BaseClosed(baseWin);
            }
        }

        public void BaseRenamed(IBaseWindow baseWin, string oldName, string newName)
        {
            // TODO: implementation of Base.SaveAs
        }

        public void ShowMDI(Form form)
        {
            if (form == null) return;

            form.MdiParent = this;
            form.Show();
        }

        public void EnableWindow(Form form, bool value)
        {
            if (form != null) {
                #if !__MonoCS__
                NativeMethods.EnableWindow(form.Handle, value);
                #endif
            }
        }

        public void Restore()
        {
            if (WindowState == FormWindowState.Minimized) {
                WindowState = FormWindowState.Normal;
            }
        }

        #endregion

        #region ISingleInstanceEnforcer implementation

        void ISingleInstanceEnforcer.OnMessageReceived(MessageEventArgs e)
        {
            OnMessageReceivedInvoker invoker = delegate(MessageEventArgs eventArgs) {
                try
                {
                    string msg = eventArgs.Message as string;

                    if (!string.IsNullOrEmpty(msg) && msg == "restore") {
                        Restore();
                    } else {
                        string[] args = eventArgs.Message as string[];
                        if (args != null) {
                            // A obligatory recovery of window, otherwise it will fail to load
                            Restore();

                            SetArgs(args);
                            LoadArgs();
                        }
                    }
                } catch (Exception ex) {
                    LogWrite("MainWin.OnMessageReceived(): " + ex.Message);
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
        }

        #endregion
    }
}
