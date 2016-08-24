/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Security.Permissions;
using System.Windows.Forms;

using Externals.SingleInstancing;
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
    public sealed partial class MainWin : Form, IHost, ISingleInstanceEnforcer
    {
        private delegate void OnMessageReceivedInvoker(MessageEventArgs e);

        private class WidgetInfo
        {
            public IWidget Widget;
            public ToolStripMenuItem MenuItem;
        }

        private NamesTable fNamesTable;
        private GlobalOptions fOptions;
        private List<IPlugin> fPlugins;
        private Timer fAutosaveTimer;

        private readonly List<WidgetInfo> fActiveWidgets;
        private string[] fCommandArgs;
        private string fLogFilename;

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

        public MainWin()
        {
            this.InitializeComponent();

            fInstance = this;
            fResourceManager = new GKResourceManager("GKResources", typeof(MainWin).Assembly);

            this.tbFileNew.Image = global::GKResources.iCreateNew;
            this.tbFileLoad.Image = global::GKResources.iLoad;
            this.tbFileSave.Image = global::GKResources.iSave;
            this.tbRecordAdd.Image = global::GKResources.iRecNew;
            this.tbRecordEdit.Image = global::GKResources.iRecEdit;
            this.tbRecordDelete.Image = global::GKResources.iRecDelete;
            this.tbFilter.Image = global::GKResources.iFilter;
            this.tbTreeAncestors.Image = global::GKResources.iTreeAncestry;
            this.tbTreeDescendants.Image = global::GKResources.iTreeDescendants;
            this.tbTreeBoth.Image = global::GKResources.iTreeBoth;
            this.tbPedigree.Image = global::GKResources.iScroll;
            this.tbStats.Image = global::GKResources.iTable;
            this.tbPrev.Image = global::GKResources.iLeft1;
            this.tbNext.Image = global::GKResources.iRight1;
            this.tbDocPreview.Image = global::GKResources.iPreview;
            this.tbDocPrint.Image = global::GKResources.iPrint;

            this.fActiveWidgets = new List<WidgetInfo>();

            this.fAutosaveTimer = new Timer(this.components);
            this.fAutosaveTimer.Stop();
            this.fAutosaveTimer.Enabled = false;
            this.fAutosaveTimer.Interval = 10 * 60 * 1000;
            this.fAutosaveTimer.Tick += this.AutosaveTimer_Tick;
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
                int num = base.MdiChildren.Length;
                for (int i = 0; i < num; i++) {
                    Form child = base.MdiChildren[i];

                    if (child is IBaseWindow) {
                        IBaseWindow baseWin = child as IBaseWindow;

                        // file is modified, isn't updated now, and isn't now created (exists)
                        if (baseWin.Modified && !baseWin.Context.IsUpdated() && !baseWin.IsUnknown()) {
                            // TODO: if file is new and not exists - don't save it, but hint to user
                            baseWin.FileSave(baseWin.Tree.FileName);
                        }
                    }
                }
            } catch (Exception ex) {
                this.LogWrite("MainWin.AutosaveTimer_Tick(): " + ex.Message);
            }
        }

        private void ApplyOptions()
        {
            this.fAutosaveTimer.Interval = this.fOptions.AutosaveInterval /* min */ * 60 * 1000;
            this.fAutosaveTimer.Enabled = this.fOptions.Autosave;
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
            this.ApplyOptions();

            if (!this.fOptions.MWinRect.IsEmpty()) {
                base.Left = this.fOptions.MWinRect.Left;
                base.Top = this.fOptions.MWinRect.Top;
                base.Width = this.fOptions.MWinRect.Right - this.fOptions.MWinRect.Left + 1;
                base.Height = this.fOptions.MWinRect.Bottom - this.fOptions.MWinRect.Top + 1;
            } else {
                base.Left = (Screen.PrimaryScreen.WorkingArea.Width - 800) / 2;
                base.Top = (Screen.PrimaryScreen.WorkingArea.Height - 600) / 2;
                base.Width = 800;
                base.Height = 600;
            }
            base.WindowState = this.fOptions.MWinState;

            this.fNamesTable = new NamesTable();
            this.fNamesTable.LoadFromFile(this.GetAppDataPath() + "GEDKeeper2.nms");

            this.fPlugins = new List<IPlugin>();
            this.LoadPlugins(GKUtils.GetPluginsPath());
            this.UpdatePluginsItems();

            this.LoadLanguage(this.fOptions.InterfaceLang);

            this.UpdateMRU();
            this.UpdateControls(false);

            this.LoadArgs();
        }

        private void Form_Closed(object sender, FormClosedEventArgs e)
        {
            try {
                this.UnloadPlugins();

                this.fOptions.MWinRect = GKUtils.GetFormRect(this);
                this.fOptions.MWinState = base.WindowState;

                this.fNamesTable.SaveToFile(this.GetAppDataPath() + "GEDKeeper2.nms");
                this.fNamesTable.Dispose();

                this.fOptions.SaveToFile(this.GetAppDataPath() + "GEDKeeper2.ini");
                this.fOptions.Dispose();
            } catch (Exception ex) {
                this.LogWrite("MainWin.Form_Closed(): " + ex.Message);
            }
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.F12) {
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
                this.LogWrite("MainWin.Form_DragDrop(): " + ex.Message);
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
            if (sender == this.tbFileNew) {
                this.miFileNewClick(null, null);
            } else if (sender == this.tbFileLoad) {
                this.miFileLoadClick(null, null);
            } else if (sender == this.tbFileSave) {
                this.miFileSaveClick(null, null);
            } else if (sender == this.tbRecordAdd) {
                this.miRecordAddClick(null, null);
            } else if (sender == this.tbRecordEdit) {
                this.miRecordEditClick(null, null);
            } else if (sender == this.tbRecordDelete) {
                this.miRecordDeleteClick(null, null);
            } else if (sender == this.tbFilter) {
                this.miFilterClick(null, null);
            } else if (sender == this.tbTreeAncestors) {
                this.miTreeAncestorsClick(null, null);
            } else if (sender == this.tbTreeDescendants) {
                this.miTreeDescendantsClick(null, null);
            } else if (sender == this.tbTreeBoth) {
                this.miTreeBothClick(null, null);
            } else if (sender == this.tbStats) {
                this.miStatsClick(null, null);
            } else if (sender == this.tbPrev) {
                this.tbPrevClick(null, null);
            } else if (sender == this.tbNext) {
                this.tbNextClick(null, null);
            } else if (sender == this.tbDocPrint) {
                this.tbDocPrintClick(null, null);
            } else if (sender == this.tbDocPreview) {
                this.tbDocPreviewClick(null, null);
            }
        }

        private void miExit_Click(object sender, EventArgs e)
        {
            base.Close();
        }

        #endregion

        #region Misc functions

        private ushort RequestLanguage()
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
            if (langCode <= 0) {
                langCode = this.RequestLanguage();
            }

            try {
                if (langCode != LangMan.LS_DEF_CODE) {
                    bool loaded = false;

                    int num = this.fOptions.GetLangsCount();
                    for (int i = 0; i < num; i++) {
                        if (this.fOptions.GetLang(i).Code == langCode) {
                            loaded = LangMan.LoadFromFile(this.fOptions.GetLang(i).FileName);
                            break;
                        }
                    }

                    if (!loaded) langCode = LangMan.LS_DEF_CODE;
                }

                if (langCode == LangMan.LS_DEF_CODE) {
                    LangMan.DefInit();
                }

                foreach (Form child in base.MdiChildren) {
                    ILocalization localChild = (child as ILocalization);

                    if (localChild != null) {
                        localChild.SetLang();
                    }
                }

                this.SetLang();

                this.fOptions.InterfaceLang = (ushort)langCode;

                this.UpdatePluginsLanguage();
            } catch (Exception ex) {
                this.LogWrite("MainWin.LoadLanguage(): " + ex.Message);
            }
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
            int idx = (int)((GKToolStripMenuItem)sender).Tag;
            this.CreateBase(this.fOptions.MRUFiles[idx].FileName);
        }

        private void UpdateMRU()
        {
            try {
                this.miMRUFiles.Enabled = (this.fOptions.MRUFiles.Count > 0);
                this.miMRUFiles.DropDownItems.Clear();
                this.MenuMRU.Items.Clear();

                int num = this.fOptions.MRUFiles.Count;
                for (int i = 0; i < num; i++) {
                    string fn = this.fOptions.MRUFiles[i].FileName;

                    GKToolStripMenuItem mi = new GKToolStripMenuItem(fn, i);
                    mi.Click += this.MRUFileClick;
                    this.miMRUFiles.DropDownItems.Add(mi);

                    GKToolStripMenuItem tsmi = new GKToolStripMenuItem(fn, i);
                    tsmi.Click += this.MRUFileClick;
                    this.MenuMRU.Items.Add(tsmi);
                }
            } catch (Exception ex) {
                this.LogWrite("MainWin.UpdateMRU(): " + ex.Message);
            }
        }

        public void AddMRU(string fileName)
        {
            int idx = this.fOptions.MRUFiles_IndexOf(fileName);

            MRUFile tmp_mf;
            if (idx >= 0) {
                tmp_mf = this.fOptions.MRUFiles[idx];
                this.fOptions.MRUFiles.RemoveAt(idx);
            } else {
                tmp_mf = new MRUFile(fileName);
            }

            this.fOptions.MRUFiles.Insert(0, tmp_mf);

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
                this.LogWrite("MainWin.UpdateNavControls(): " + ex.Message);
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
                this.LogWrite("MainWin.UpdateControls(): " + ex.Message);
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
                result.Activate();
                return result;
            }

            result = new BaseWin();
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
                        ((IBaseWindow)child).CriticalSave();
                    }
                }
            } catch (Exception ex) {
                this.LogWrite("MainWin.CriticalSave(): " + ex.Message);
            }
        }

        private void miUndoClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase != null)
            {
                curBase.Context.DoUndo();
            }
        }

        private void miRedoClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase != null)
            {
                curBase.Context.DoRedo();
            }
        }

        private void miExportToFamilyBookClick(object sender, EventArgs e)
        {
            #if __MonoCS__
            this.ShowWarning(@"This function is not supported in Linux");
            #else
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            using (FamilyBookExporter fb = new FamilyBookExporter(curBase)) {
                fb.Generate(true);
            }
            #endif
        }

        private void miExportToExcelFileClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            using (ExcelExporter exExp = new ExcelExporter(curBase)) {
                exExp.Options = this.fOptions;
                exExp.SelectedRecords = curBase.GetContentList(GEDCOMRecordType.rtIndividual);
                exExp.Generate(true);
            }
        }

        private void miFilePropertiesClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (FilePropertiesDlg dlgFileProps = new FilePropertiesDlg(curBase)) {
                    this.ShowModalEx(dlgFileProps, false);
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miScriptsClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (ScriptEditWin dmn = new ScriptEditWin(curBase)) {
                    this.ShowModalEx(dmn, false);
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miTreeToolsClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (TreeToolsWin fmTreeTools = new TreeToolsWin(curBase)) {
                    this.ShowModalEx(fmTreeTools, false);
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miOptionsClick(object sender, EventArgs e)
        {
            using (OptionsDlg dlgOptions = new OptionsDlg(this))
            {
                Form activeForm = this.ActiveMdiChild;
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
                    this.ApplyOptions();

                    foreach (Form child in base.MdiChildren)
                    {
                        if (child is IWorkWindow) {
                            (child as IWorkWindow).UpdateView();
                        }
                    }
                }
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
            string fileName = UIHelper.GetOpenFile("", this.fOptions.LastDir, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName)) {
                this.CreateBase(fileName);
            }
        }

        public void miFileSaveClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile(true);
            if (curBase == null) return;

            string fileName = UIHelper.GetSaveFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT, curBase.Tree.FileName, false);
            if (!string.IsNullOrEmpty(fileName)) {
                curBase.FileSave(fileName);
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
            #if __MonoCS__
            this.ShowWarning(@"This function is not supported in Linux");
            #else
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;
            
            MapsViewerWin mapsWin = new MapsViewerWin(curBase);
            mapsWin.MdiParent = this;
            mapsWin.ProcessMap();
            #endif
        }

        private void miOrganizerClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            using (OrganizerWin dlg = new OrganizerWin(curBase)) {
                this.ShowModalEx(dlg, false);
            }
        }

        private void miSlideshowClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            SlideshowWin dlg = new SlideshowWin(curBase);
            dlg.MdiParent = this;
            dlg.Show();
        }

        private void miStatsClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            List<GEDCOMRecord> selectedRecords = curBase.GetContentList(GEDCOMRecordType.rtIndividual);

            StatisticsWin fmStats = new StatisticsWin(curBase, selectedRecords);
            fmStats.Show();
        }

        private void GeneratePedigree(PedigreeExporter.PedigreeKind kind)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            using (PedigreeExporter p = new PedigreeExporter(curBase)) {
                p.Root = curBase.GetSelectedPerson();
                p.Options = this.fOptions;
                p.ShieldState = curBase.ShieldState;
                p.Kind = kind;
                p.Generate(true);
            }
        }

        private void miPedigreeAscend_Click(object sender, EventArgs e)
        {
            this.GeneratePedigree(PedigreeExporter.PedigreeKind.pkAscend);
        }

        private void miPedigree_dAbovilleClick(object sender, EventArgs e)
        {
            this.GeneratePedigree(PedigreeExporter.PedigreeKind.pkDescend_dAboville);
        }

        private void miPedigree_KonovalovClick(object sender, EventArgs e)
        {
            this.GeneratePedigree(PedigreeExporter.PedigreeKind.pkDescend_Konovalov);
        }

        private void miTreeAncestorsClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            if (TreeChartWin.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartBox.ChartKind.ckAncestors)) {
                TreeChartWin fmChart = new TreeChartWin(curBase, curBase.GetSelectedPerson());
                fmChart.ChartKind = TreeChartBox.ChartKind.ckAncestors;
                fmChart.GenChart(true);
            }
        }

        private void miTreeDescendantsClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            if (TreeChartWin.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartBox.ChartKind.ckDescendants)) {
                TreeChartWin fmChart = new TreeChartWin(curBase, curBase.GetSelectedPerson());
                fmChart.ChartKind = TreeChartBox.ChartKind.ckDescendants;
                fmChart.GenChart(true);
            }
        }

        private void miTreeBothClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            if (TreeChartWin.CheckData(curBase.Tree, curBase.GetSelectedPerson(), TreeChartBox.ChartKind.ckBoth)) {
                TreeChartWin fmChart = new TreeChartWin(curBase, curBase.GetSelectedPerson());
                fmChart.ChartKind = TreeChartBox.ChartKind.ckBoth;
                fmChart.GenChart(true);
            }
        }

        private void miAncestorsCircleClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            CircleChartWin fmChart = new CircleChartWin(curBase, curBase.GetSelectedPerson(), CircleChartType.Ancestors);
            fmChart.GenChart(true);
        }

        private void miDescendantsCircleClick(object sender, EventArgs e)
        {
            IBaseWindow curBase = this.GetCurrentFile();
            if (curBase == null) return;

            CircleChartWin fmChart = new CircleChartWin(curBase, curBase.GetSelectedPerson(), CircleChartType.Descendants);
            fmChart.GenChart(true);
        }

        #endregion

        #region Help and Windows

        private void miLogSendClick(object sender, EventArgs e)
        {
            SysUtils.SendMail(GKData.APP_MAIL, "GEDKeeper: error notification", "This automatic notification of error.", this.fLogFilename);
        }

        private void miLogViewClick(object sender, EventArgs e)
        {
            SysUtils.LoadExtFile(this.fLogFilename);
        }

        private void miAboutClick(object sender, EventArgs e)
        {
            AboutDlg.ShowAbout();
        }

        public void ShowHelpTopic(string topic)
        {
            string lngSign;

            LangRecord lngrec = this.fOptions.GetLangByCode(this.fOptions.InterfaceLang);
            if (lngrec == null) {
                if (this.fOptions.InterfaceLang == LangMan.LS_DEF_CODE) {
                    lngSign = LangMan.LS_DEF_SIGN;
                } else {
                    return;
                }
            } else {
                lngSign = lngrec.Sign;
            }

            string helpPath = GKUtils.GetHelpPath(lngSign);

            if (string.IsNullOrEmpty(topic)) {
                topic = helpPath + "GEDKeeper2.html";
            } else {
                topic = helpPath + topic;
            }

            if (!File.Exists(topic)) {
                GKUtils.ShowError(@"For that language help is unavailable");
                return;
            }

            SysUtils.LoadExtFile(topic);
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

        private void miWindowDropDownOpening(object sender, EventArgs e)
        {
            Form activeChild = this.ActiveMdiChild;
            if (activeChild != null)
            {
                // platform: in Mono here is bug, but code works without this line
                #if !__MonoCS__
                ActivateMdiChild(null);
                #endif

                ActivateMdiChild(activeChild);
            }
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

            this.miPedigreeAscend.Text = LangMan.LS(LSID.LSID_MIPedigreeAscend);
            this.miPedigree_dAboville.Text = LangMan.LS(LSID.LSID_MIPedigree_dAboville);
            this.miPedigree_Konovalov.Text = LangMan.LS(LSID.LSID_MIPedigree_Konovalov);

            this.miMap.Text = LangMan.LS(LSID.LSID_MIMap) + @"...";
            this.miStats.Text = LangMan.LS(LSID.LSID_MIStats) + @"...";

            this.miOrganizer.Text = LangMan.LS(LSID.LSID_MIOrganizer) + @"...";
            this.miSlideshow.Text = LangMan.LS(LSID.LSID_Slideshow) + @"...";
            this.miScripts.Text = LangMan.LS(LSID.LSID_MIScripts);
            this.miTreeTools.Text = LangMan.LS(LSID.LSID_MITreeTools);
            this.miFilter.Text = LangMan.LS(LSID.LSID_MIFilter) + @"...";
            this.miOptions.Text = LangMan.LS(LSID.LSID_MIOptions) + @"...";

            this.miWinCascade.Text = LangMan.LS(LSID.LSID_MIWinCascade);
            this.miWinHTile.Text = LangMan.LS(LSID.LSID_MIWinHTile);
            this.miWinVTile.Text = LangMan.LS(LSID.LSID_MIWinVTile);
            this.miWinMinimize.Text = LangMan.LS(LSID.LSID_MIWinMinimize);
            this.miWinArrange.Text = LangMan.LS(LSID.LSID_MIWinArrange);

            //this.miGenResources.Text = LangMan.LS(LSID.LSID_MIGenResources);
            //this.miKinshipTerms.Text = LangMan.LS(LSID.LSID_MIKinshipTerms);
            //this.miFAQ.Text = LangMan.LS(LSID.LSID_MIFAQ);
            this.miContext.Text = LangMan.LS(LSID.LSID_MIContext);
            this.miAbout.Text = LangMan.LS(LSID.LSID_MIAbout) + @"...";

            this.miLogSend.Text = LangMan.LS(LSID.LSID_LogSend);
            this.miLogView.Text = LangMan.LS(LSID.LSID_LogView);
            this.miPlugins.Text = LangMan.LS(LSID.LSID_Plugins);

            this.miSearch.Text = LangMan.LS(LSID.LSID_Search);
            this.miAncestorsCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
            this.miDescendantsCircle.Text = LangMan.LS(LSID.LSID_DescendantsCircle);

            this.tbFileNew.ToolTipText = LangMan.LS(LSID.LSID_FileNewTip);
            this.tbFileLoad.ToolTipText = LangMan.LS(LSID.LSID_FileLoadTip);
            this.tbFileSave.ToolTipText = LangMan.LS(LSID.LSID_FileSaveTip);
            this.tbRecordAdd.ToolTipText = LangMan.LS(LSID.LSID_RecordAddTip);
            this.tbRecordEdit.ToolTipText = LangMan.LS(LSID.LSID_RecordEditTip);
            this.tbRecordDelete.ToolTipText = LangMan.LS(LSID.LSID_RecordDeleteTip);
            this.tbFilter.ToolTipText = LangMan.LS(LSID.LSID_FilterTip);
            this.tbTreeAncestors.ToolTipText = LangMan.LS(LSID.LSID_TreeAncestorsTip);
            this.tbTreeDescendants.ToolTipText = LangMan.LS(LSID.LSID_TreeDescendantsTip);
            this.tbTreeBoth.ToolTipText = LangMan.LS(LSID.LSID_TreeBothTip);
            this.tbPedigree.ToolTipText = LangMan.LS(LSID.LSID_PedigreeTip);
            this.miPedigree_dAboville2.Text = LangMan.LS(LSID.LSID_Pedigree_dAbovilleTip);
            this.miPedigree_Konovalov2.Text = LangMan.LS(LSID.LSID_Pedigree_KonovalovTip);
            this.tbStats.ToolTipText = LangMan.LS(LSID.LSID_StatsTip);

            this.tbDocPrint.ToolTipText = LangMan.LS(LSID.LSID_DocPrint);
            this.tbDocPreview.ToolTipText = LangMan.LS(LSID.LSID_DocPreview);

            this.tbPrev.ToolTipText = LangMan.LS(LSID.LSID_PrevRec);
            this.tbNext.ToolTipText = LangMan.LS(LSID.LSID_NextRec);
        }

        #endregion

        #region Plugins support

        public static PluginInfo GetPluginAttributes(IPlugin plugin)
        {
            if (plugin == null) {
                throw new ArgumentNullException("plugin");
            }

            PluginInfo info = new PluginInfo();

            Assembly asm = plugin.GetType().Assembly;

            var attr1 = SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(asm);
            if (attr1 != null) info.Title = attr1.Title;

            var attr2 = SysUtils.GetAssemblyAttribute<AssemblyDescriptionAttribute>(asm);
            if (attr2 != null) info.Description = attr2.Description;

            var attr3 = SysUtils.GetAssemblyAttribute<AssemblyCopyrightAttribute>(asm);
            if (attr3 != null) info.Copyright = attr3.Copyright;

            var attr4 = SysUtils.GetAssemblyAttribute<AssemblyFileVersionAttribute>(asm);
            if (attr4 != null) info.Version = attr4.Version;

            return info;
        }

        private void UpdatePluginsLanguage()
        {
            if (this.fPlugins == null) return;

            foreach (IPlugin plugin in this.fPlugins) {
                plugin.OnLanguageChange();
            }

            int num = this.miPlugins.DropDownItems.Count;
            for (int i = 0; i < num; i++) {
                ToolStripItem mi = this.miPlugins.DropDownItems[i];
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
                this.miPlugins.Visible = (this.fPlugins.Count > 0);
                this.miPlugins.DropDownItems.Clear();

                this.fActiveWidgets.Clear();

                foreach (IPlugin plugin in this.fPlugins) {
                    string dispName = plugin.DisplayName;

                    ToolStripMenuItem mi = new ToolStripMenuItem(dispName/*, i*/);
                    mi.Click += Plugin_Click;
                    mi.Tag = plugin;
                    this.miPlugins.DropDownItems.Add(mi);

                    if (plugin is IWidget) {
                        WidgetInfo widInfo = new WidgetInfo();
                        widInfo.Widget = (plugin as IWidget);
                        widInfo.MenuItem = mi;
                        this.fActiveWidgets.Add(widInfo);

                        (plugin as IWidget).WidgetInit(this);
                    }
                }
            } catch (Exception ex) {
                this.LogWrite("MainWin.UpdatePluginsItems(): " + ex.Message);
            }
        }

        private void UnloadPlugins()
        {
            try {
                if (this.fPlugins == null) return;

                foreach (IPlugin plugin in this.fPlugins) {
                    plugin.Shutdown();
                }
            } catch (Exception ex) {
                this.LogWrite("MainWin.UnloadPlugins(): " + ex.Message);
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
                            this.fPlugins.Add(plugin);
                        }
                    } catch (Exception ex) {
                        this.LogWrite("MainWin.LoadPlugin(" + pfn + "): " + ex.Message);
                    }
                }
            } catch (Exception ex) {
                this.LogWrite("MainWin.LoadPlugins(" + path + "): " + ex.Message);
            }
        }

        #endregion

        #region IHost implementation

        public bool IsUnix()
        {
            return SysInfo.IsUnix();
        }

        public void ShowWarning(string msg)
        {
            MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.OK, MessageBoxIcon.Warning);
        }

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
            return (res) ? langMan : null;
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

            foreach (IPlugin plugin in this.fPlugins) {
                ISubscriber subscriber = (plugin as ISubscriber);
                if (subscriber != null) {
                    try {
                        subscriber.NotifyRecord(aBase, record, action);
                    } catch (Exception ex) {
                        Logger.LogWrite("MainWin.NotifyRecord(): " + ex.Message);
                    }
                }
            }
        }

        public string GetAppDataPath()
        {
            string path = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + Path.DirectorySeparatorChar + GKData.APP_TITLE + Path.DirectorySeparatorChar;
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
                    MessageBox.Show(msg, @"Message from new instance", MessageBoxButtons.OK, MessageBoxIcon.Information);
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
