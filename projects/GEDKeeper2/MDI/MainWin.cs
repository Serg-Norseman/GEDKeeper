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
using System.Security.Permissions;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Charts;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKUI.Components;
using GKUI.Dialogs;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class MainWin : Form, IMainWindow
    {
        private readonly Timer fAutosaveTimer;

        public MainWin()
        {
            InitializeComponent();

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

            fAutosaveTimer = new Timer(components);
            fAutosaveTimer.Stop();
            fAutosaveTimer.Enabled = false;
            fAutosaveTimer.Interval = 10 * 60 * 1000;
            fAutosaveTimer.Tick += AutosaveTimer_Tick;

            //LangMan.SaveDefaultLanguage();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                AppHost.Options.Dispose();

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
                        if (baseWin.Modified && !baseWin.Context.IsUpdated() && !baseWin.Context.IsUnknown()) {
                            // TODO: if file is new and not exists - don't save it, but hint to user
                            baseWin.SaveFile(baseWin.Context.FileName);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("MainWin.AutosaveTimer_Tick(): " + ex.Message);
            }
        }

        private void ApplyOptions()
        {
            fAutosaveTimer.Interval = AppHost.Options.AutosaveInterval /* min */ * 60 * 1000;
            fAutosaveTimer.Enabled = AppHost.Options.Autosave;
        }

        private void Form_Show(object sender, EventArgs e)
        {
            try
            {
                try
                {
                    AppHost.Instance.BeginLoading();

                    AppHost.Instance.ReloadRecentBases();

                    AppHost.Instance.ProcessHolidays();
                } finally {
                    AppHost.Instance.EndLoading();
                }

                UpdateMan.CheckUpdate();
            } catch (Exception ex) {
                Logger.LogWrite("MainWin.Form_Show(): " + ex.Message);
            }
        }

        private void Form_Load(object sender, EventArgs e)
        {
            try
            {
                ApplyOptions();
                RestoreWindowState();
                UpdatePluginsItems();
                AppHost.Instance.LoadLanguage(AppHost.Options.InterfaceLang);
                UpdateMRU();
                UpdateControls(false);
                AppHost.Instance.LoadArgs();
            } catch (Exception ex) {
                Logger.LogWrite("MainWin.Form_Load(): " + ex.Message);
            }
        }

        private void Form_Closed(object sender, FormClosedEventArgs e)
        {
            try {
                SaveWindowState();
            } catch (Exception ex) {
                Logger.LogWrite("MainWin.Form_Closed(): " + ex.Message);
            }
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.F12) {
                #if __MonoCS__
                AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
                #else
                IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
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
            AppHost.Options.ClearLastBases();
            for (int i = MdiChildren.Length - 1; i >= 0; i--) {
                Form mdiChild = MdiChildren[i];
                if (mdiChild is IBaseWindow) {
                    AppHost.Options.AddLastBase((mdiChild as IBaseWindow).Context.FileName);
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
                    AppHost.Instance.BeginLoading();

                    Array a = e.Data.GetData(DataFormats.FileDrop) as Array;
                    if (a == null) return;

                    for (int i = 0; i < a.Length; i++) {
                        string fn = a.GetValue(i).ToString();
                        AppHost.Instance.CreateBase(fn);
                    }
                } finally {
                    AppHost.Instance.EndLoading();
                }
            } catch (Exception ex) {
                Logger.LogWrite("MainWin.Form_DragDrop(): " + ex.Message);
            }
        }

        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        protected override void WndProc(ref Message m)
        {
            base.WndProc(ref m);

            if (m.Msg == NativeMethods.WM_KEEPMODELESS) {
                foreach (WidgetInfo widgetInfo in AppHost.Instance.ActiveWidgets) {
                    widgetInfo.Widget.WidgetEnable();
                }
            }
        }

        private void StatusBar_DrawItem(object sender, StatusBarDrawItemEventArgs sbdevent)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            Bitmap pic = null;
            switch (curBase.Context.ShieldState)
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
                IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
                if (curBase != null) {
                    curBase.Context.SwitchShieldState();
                    StatusBar.Invalidate();
                }
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

        private void SaveWindowState()
        {
            AppHost.Options.MWinRect = UIHelper.GetFormRect(this);
            AppHost.Options.MWinState = (WindowState)this.WindowState;
        }

        private void RestoreWindowState()
        {
            try
            {
                ExtRect mwinRect = AppHost.Options.MWinRect;

                UIHelper.NormalizeFormRect(ref mwinRect);

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
                WindowState = (FormWindowState)AppHost.Options.MWinState;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("MainWin.RestoreWindowState(): " + ex.Message);
            }
        }

        public void Restore()
        {
            if (WindowState == FormWindowState.Minimized) {
                WindowState = FormWindowState.Normal;
            }
        }

        private void MRUFileClick(object sender, EventArgs e)
        {
            int idx = (int)((GKToolStripMenuItem)sender).Tag;
            AppHost.Instance.CreateBase(AppHost.Options.MRUFiles[idx].FileName);
        }

        public void UpdateMRU()
        {
            try {
                miMRUFiles.Enabled = (AppHost.Options.MRUFiles.Count > 0);
                miMRUFiles.DropDownItems.Clear();
                MenuMRU.Items.Clear();

                int num = AppHost.Options.MRUFiles.Count;
                for (int i = 0; i < num; i++) {
                    string fn = AppHost.Options.MRUFiles[i].FileName;

                    GKToolStripMenuItem mi = new GKToolStripMenuItem(fn, i);
                    mi.Click += MRUFileClick;
                    miMRUFiles.DropDownItems.Add(mi);

                    GKToolStripMenuItem tsmi = new GKToolStripMenuItem(fn, i);
                    tsmi.Click += MRUFileClick;
                    MenuMRU.Items.Add(tsmi);
                }
            } catch (Exception ex) {
                Logger.LogWrite("MainWin.UpdateMRU(): " + ex.Message);
            }
        }

        public void CheckMRUWin(string fileName, Form frm)
        {
            int idx = AppHost.Options.MRUFiles_IndexOf(fileName);
            if (idx < 0) return;

            MRUFile mf = AppHost.Options.MRUFiles[idx];
            mf.WinRect = UIHelper.GetFormRect(frm);
            mf.WinState = (WindowState)frm.WindowState;
        }

        public void UpdateNavControls()
        {
            try
            {
                IWorkWindow workWin = AppHost.Instance.GetWorkWindow();

                tbPrev.Enabled = (workWin != null && workWin.NavCanBackward());
                tbNext.Enabled = (workWin != null && workWin.NavCanForward());
            } catch (Exception ex) {
                Logger.LogWrite("MainWin.UpdateNavControls(): " + ex.Message);
            }
        }

        public void UpdateControls(bool forceDeactivate)
        {
            try
            {
                IBaseWindow curBase = ((forceDeactivate) ? null : AppHost.Instance.GetCurrentFile());
                IChartWindow curChart = ((ActiveMdiChild is IChartWindow) ? ((IChartWindow) ActiveMdiChild) : null);

                IWorkWindow workWin = AppHost.Instance.GetWorkWindow();

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
                Logger.LogWrite("MainWin.UpdateControls(): " + ex.Message);
            }
        }

        private void miExit_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void miUndo_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            curBase.Context.DoUndo();
        }

        private void miRedo_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            curBase.Context.DoRedo();
        }

        private void miExportToFamilyBook_Click(object sender, EventArgs e)
        {
            #if __MonoCS__
            AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            #else
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            using (FamilyBookExporter fb = new FamilyBookExporter(curBase)) {
                fb.Generate(true);
            }
            #endif
        }

        private void miExportToExcelFile_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            using (ExcelExporter exExp = new ExcelExporter(curBase)) {
                exExp.Options = AppHost.Options;
                exExp.Generate(true);
            }
        }

        private void miFileProperties_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (var dlg = new FilePropertiesDlg()) {
                    dlg.InitDialog(curBase);
                    AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miScripts_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (ScriptEditWin scriptWin = new ScriptEditWin(curBase)) {
                    scriptWin.ShowDialog();
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miTreeTools_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            try {
                curBase.Context.BeginUpdate();

                using (TreeToolsWin fmTreeTools = new TreeToolsWin(curBase)) {
                    fmTreeTools.ShowDialog();
                }
            } finally {
                curBase.Context.EndUpdate();
            }
        }

        private void miOptions_Click(object sender, EventArgs e)
        {
            using (OptionsDlg dlgOptions = new OptionsDlg(AppHost.Instance))
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
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            curBase.Close();
        }

        private void miFileNew_Click(object sender, EventArgs e)
        {
            AppHost.Instance.CreateBase("");
        }

        private void miFileLoad_Click(object sender, EventArgs e)
        {
            string homePath = AppHost.Instance.GetUserFilesPath("");

            string fileName = AppHost.StdDialogs.GetOpenFile("", homePath, LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (!string.IsNullOrEmpty(fileName)) {
                AppHost.Instance.CreateBase(fileName);
            }
        }

        private void miFileSaveAs_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile(true);
            if (curBase == null) return;

            curBase.SaveFileEx(true);
        }

        private void miFileSave_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile(true);
            if (curBase == null) return;

            curBase.SaveFileEx(false);
        }

        private void miRecordAdd_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            curBase.AddRecord();
        }

        private void miRecordEdit_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            curBase.EditRecord();
        }

        private void miRecordDelete_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            curBase.DeleteRecord();
        }

        private void miSearch_Click(object sender, EventArgs e)
        {
            IWorkWindow win = AppHost.Instance.GetWorkWindow();
            if (win == null) return;

            win.QuickSearch();
        }

        private void miFilter_Click(object sender, EventArgs e)
        {
            IWorkWindow win = AppHost.Instance.GetWorkWindow();
            if (win == null) return;

            win.SetFilter();
        }

        private void tbPrev_Click(object sender, EventArgs e)
        {
            IWorkWindow win = AppHost.Instance.GetWorkWindow();
            if (win == null) return;

            win.NavPrev();
        }

        private void tbNext_Click(object sender, EventArgs e)
        {
            IWorkWindow win = AppHost.Instance.GetWorkWindow();
            if (win == null) return;

            win.NavNext();
        }

        private void tbDocPrint_Click(object sender, EventArgs e)
        {
            IChartWindow chartWin = AppHost.Instance.GetWorkWindow() as IChartWindow;
            if (chartWin != null && chartWin.AllowPrint()) {
                chartWin.DoPrint();
            }
        }

        private void tbDocPreview_Click(object sender, EventArgs e)
        {
            IChartWindow chartWin = AppHost.Instance.GetWorkWindow() as IChartWindow;
            if (chartWin != null && chartWin.AllowPrint()) {
                chartWin.DoPrintPreview();
            }
        }

        private void miMap_Click(object sender, EventArgs e)
        {
            #if __MonoCS__
            AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
            #else
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;
            
            MapsViewerWin mapsWin = new MapsViewerWin(curBase);
            mapsWin.MdiParent = this;
            mapsWin.ProcessMap();
            #endif
        }

        private void miOrganizer_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            using (OrganizerWin dlg = new OrganizerWin(curBase)) {
                dlg.ShowDialog();
            }
        }

        private void miRelationshipCalculator_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            using (RelationshipCalculatorDlg relCalc = new RelationshipCalculatorDlg(curBase)) {
                relCalc.ShowDialog();
            }
        }

        private void miSlideshow_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            SlideshowWin win = new SlideshowWin(curBase);
            AppHost.Instance.ShowWindow(win, false);
        }

        private void miStats_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            List<GEDCOMRecord> selectedRecords = curBase.GetContentList(GEDCOMRecordType.rtIndividual);

            StatisticsWin win = new StatisticsWin(curBase, selectedRecords);
            AppHost.Instance.ShowWindow(win, false);
        }

        private void GeneratePedigree(PedigreeExporter.PedigreeKind kind)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            using (PedigreeExporter p = new PedigreeExporter(curBase)) {
                p.Root = curBase.GetSelectedPerson();
                p.Options = AppHost.Options;
                p.ShieldState = curBase.Context.ShieldState;
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
            ShowTreeChart(TreeChartKind.ckAncestors);
        }

        private void miTreeDescendants_Click(object sender, EventArgs e)
        {
            ShowTreeChart(TreeChartKind.ckDescendants);
        }

        private void miTreeBoth_Click(object sender, EventArgs e)
        {
            ShowTreeChart(TreeChartKind.ckBoth);
        }

        private void ShowTreeChart(TreeChartKind chartKind)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            GEDCOMIndividualRecord selPerson = curBase.GetSelectedPerson();
            if (TreeChartModel.CheckTreeChartSize(curBase.Context.Tree, selPerson, chartKind)) {
                TreeChartWin fmChart = new TreeChartWin(curBase, selPerson);
                fmChart.ChartKind = chartKind;
                fmChart.GenChart();
                AppHost.Instance.ShowWindow(fmChart, false);
            }
        }

        private void miAncestorsCircle_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            CircleChartWin fmChart = new CircleChartWin(curBase, curBase.GetSelectedPerson(), CircleChartType.Ancestors);
            AppHost.Instance.ShowWindow(fmChart, false);
        }

        private void miDescendantsCircle_Click(object sender, EventArgs e)
        {
            IBaseWindow curBase = AppHost.Instance.GetCurrentFile();
            if (curBase == null) return;

            CircleChartWin fmChart = new CircleChartWin(curBase, curBase.GetSelectedPerson(), CircleChartType.Descendants);
            AppHost.Instance.ShowWindow(fmChart, false);
        }

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
            using (AboutDlg dlg = new AboutDlg()) {
                dlg.ShowDialog();
            }
        }

        private void miContext_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowHelpTopic("");
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
                miPlugins.Visible = (AppHost.Plugins.Count > 0);
                miPlugins.DropDownItems.Clear();

                AppHost.Instance.ActiveWidgets.Clear();

                int num = AppHost.Plugins.Count;
                for (int i = 0; i < num; i++) {
                    IPlugin plugin = AppHost.Plugins[i];
                    string dispName = plugin.DisplayName;

                    ToolStripMenuItemEx mi = new ToolStripMenuItemEx(dispName/*, i*/);
                    mi.Click += Plugin_Click;
                    mi.Tag = plugin;
                    miPlugins.DropDownItems.Add(mi);

                    if (plugin is IWidget) {
                        WidgetInfo widInfo = new WidgetInfo();
                        widInfo.Widget = (plugin as IWidget);
                        widInfo.MenuItem = mi;
                        AppHost.Instance.ActiveWidgets.Add(widInfo);

                        (plugin as IWidget).WidgetInit(AppHost.Instance);
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("MainWin.UpdatePluginsItems(): " + ex.Message);
            }
        }
    }
}
