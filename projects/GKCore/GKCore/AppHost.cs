/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.IO;
using System.Net;
using System.Reflection;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.IoC;
using GKCore.Maps;
using GKCore.Names;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.SingleInstance;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore
{
    internal delegate void OnMessageReceivedInvoker(MessageEventArgs e);


    public enum WinLayout
    {
        Cascade = 0,
        TileHorizontal = 1,
        TileVertical = 2,
        Minimize = 3,
    }


    /// <summary>
    /// Global controller of UI for the isolation
    /// of presentation from logic and data model (based on IoC).
    /// </summary>
    public abstract class AppHost : IHost, ISingleInstanceEnforcer
    {
#if CI_MODE
        public static bool TEST_MODE = true;
#else
        public static bool TEST_MODE = false;
#endif


        private static string fAppSign;
        private static AppHost fInstance = null;
        private static string fAppDataPath = null;

        private readonly List<WidgetInfo> fActiveWidgets;
        private readonly List<object> fInternalClipboard;
        private readonly StringList fTips;

        protected IBaseWindow fActiveBase;
        protected IList<IWindow> fRunningForms;

        private ITimer fAutosaveTimer;
        private string[] fCommandArgs;
        private int fLoadingCount;
        private LocalesCollection fLocalesCollection;


        public static AppHost Instance
        {
            get { return fInstance; }
        }


        public IList<WidgetInfo> ActiveWidgets
        {
            get { return fActiveWidgets; }
        }

        public LocalesCollection LocalesCollection
        {
            get { return fLocalesCollection; }
        }


        protected AppHost()
        {
            fInstance = this;

            fActiveWidgets = new List<WidgetInfo>();
            fInternalClipboard = new List<object>();
            fRunningForms = new List<IWindow>();
            fTips = new StringList();
            fLocalesCollection = new LocalesCollection();
        }

        protected virtual void ApplicationExit()
        {
            AppHost.Instance.SaveLastBases();
        }

        private void AutosaveTimer_Tick(object sender, EventArgs e)
        {
            try {
                foreach (var baseWin in GetRunningForms<IBaseWindow>()) {
                    baseWin.CheckAutosave();
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.AutosaveTimer_Tick()", ex);
            }
        }

        public static void LogSysInfo()
        {
            try {
                Logger.WriteInfo("OS Version: " + SysUtils.GetOSType().ToString());

#if MONO
                Logger.WriteInfo("Mono Version: " + SysUtils.GetMonoVersion());
#endif
#if OS_LINUX || OS_FREEBSD
                Logger.WriteInfo("Desktop Type: " + SysUtils.GetDesktopType().ToString());
#endif

                var execAssembly = SysUtils.GetExecutingAssembly();
                Logger.WriteInfo("CLR Version: " + execAssembly.ImageRuntimeVersion);
                Logger.WriteInfo("GK Version: " + execAssembly.GetName().Version.ToString());
            } catch {
                // dummy
            }
        }

        public virtual async Task Init(string[] args, bool isMDI)
        {
            GKUtils.InitGEDCOM();
            await LoadLanguage(AppHost.Options.InterfaceLang, true);
            SetArgs(args);
            StartupWork();
        }

        public virtual async void StartupWork()
        {
            if (HasFeatureSupport(Feature.Themes)) {
                ThemeManager.LoadThemes();
                ThemeManager.SetTheme(GlobalOptions.Instance.Theme);
            }

            try {
                ApplyOptions();

                try {
                    BeginLoading();

                    int result = await LoadArgs(false);
                    result += await ReloadRecentBases();
                    if (result == 0) {
                        await CreateBase("");
                    }

                    ProcessHolidays();
                    ProcessTips();
                    ProcessLocales();

                    await EndLoading();
                } finally {
                }

                if (Options.AutoCheckUpdates) {
                    UpdateMan.CheckUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.StartupWork()", ex);
            }
        }

        public void SetArgs(string[] args)
        {
            if (args != null) {
                fCommandArgs = (string[])args.Clone();
            }
        }

        public virtual void LoadWindow(IWindow window)
        {
            fRunningForms.Add(window);
        }

        public virtual void CloseWindow(IWindow window)
        {
            fRunningForms.Remove(window);

            if (fRunningForms.Count == 0 && !HasFeatureSupport(Feature.Mobile)) {
                Quit();
            }
        }

        public virtual IEnumerable<T> GetRunningForms<T>() where T : class
        {
            for (int i = 0, num = fRunningForms.Count; i < num; i++) {
                T form = fRunningForms[i] as T;
                if (form != null) {
                    yield return form;
                }
            }
        }

        public string GetCurrentFileName()
        {
            IBaseWindow cb = GetCurrentFile();
            return (cb == null) ? "" : cb.Context.FileName;
        }

        public void BeginLoading()
        {
            fLoadingCount++;
        }

        public async Task EndLoading()
        {
            fLoadingCount--;

            if (fLoadingCount == 0) {
                await ShowTips();
            }
        }

        public async Task ShowTips()
        {
            if (fTips.Count <= 0) return;

            using (var dlg = fIocContainer.Resolve<IDayTipsDlg>()) {
                dlg.Init(LangMan.LS(LSID.BirthDays), AppHost.Options.ShowTips, fTips);
                await ShowModalAsync(dlg, null, false);
                AppHost.Options.ShowTips = dlg.ShowTipsChecked;
            }

            fTips.Clear();
        }

        public virtual void Activate()
        {
            // May have a desktop-only implementation
        }

        public abstract IForm GetActiveForm();

        public abstract IWindow GetActiveWindow();

        public abstract IntPtr GetTopWindowHandle();

        public void ShowWindow(IWindow window)
        {
            if (window != null) {
                window.Show(true);
            }
        }

        public virtual void EnableWindow(IWidgetForm form, bool value)
        {
            // May have a desktop-only implementation
        }

        public MRUFile GetMRUFile(IBaseWindow baseWin)
        {
            if (baseWin == null) return null;

            int idx = Options.MRUFiles_IndexOf(baseWin.Context.FileName);
            if (idx < 0) return null;

            return Options.MRUFiles[idx];
        }

        public void SaveWinMRU(IBaseWindow baseWin)
        {
            var mf = GetMRUFile(baseWin);
            if (mf != null) SaveWinState(baseWin, mf);
        }

        public virtual void SaveWinState(IBaseWindow baseWin, MRUFile mf)
        {
            // May have a desktop-only implementation
        }

        public virtual void RestoreWinState(IBaseWindow baseWin, MRUFile mf)
        {
            // May have a desktop-only implementation
        }

        public void RestoreWinMRU(IBaseWindow baseWin)
        {
            var mf = GetMRUFile(baseWin);
            if (mf != null) RestoreWinState(baseWin, mf);
        }

        protected virtual void UpdateLang()
        {
            foreach (var win in GetRunningForms<IWindow>()) {
                win.SetLocale();
            }
        }

        protected void UpdateMRU()
        {
            foreach (var baseWin in GetRunningForms<IBaseWindow>()) {
                baseWin.UpdateMRU();
            }
        }

        public void SaveLastBases()
        {
#if !CI_MODE
            var lastBases = AppHost.Options.LastBases;

            lastBases.Clear();
            foreach (var baseWin in GetRunningForms<IBaseWindow>()) {
                lastBases.Add(baseWin.Context.FileName);
            }
#endif
        }

        public virtual int GetKeyLayout()
        {
            // May have a desktop-only implementation
            return 0;
        }

        public virtual void SetKeyLayout(int layout)
        {
            // May have a desktop-only implementation
        }

        public abstract ITimer CreateTimer(double msInterval, EventHandler elapsedHandler);

        public abstract void Quit();

        public abstract void ExecuteWork(ProgressStart proc);

        public abstract bool ExecuteWorkExt(ProgressStart proc, string title);

        public virtual void LayoutWindows(WinLayout layout)
        {
            // May have a desktop-only implementation
        }

        public virtual string GetExternalStorageDirectory()
        {
            // May have a mobile-only implementation
            throw new NotImplementedException();
        }

        #region Extended clipboard functions

        public abstract void SetClipboardText(string text);

        public void SetClipboardObj<T>(object obj) where T : class
        {
            /*string objName = obj.GetType().FullName;
            string objContent = GEDCOMProvider.GetTagStreamText(obj, 1, false);
            AppHost.Instance.SetClipboardObject(objName, objContent);*/

            if (obj == null)
                return;

            for (int i = fInternalClipboard.Count - 1; i >= 0; i--) {
                var item = fInternalClipboard[i];
                if (item.GetType().IsDerivedFromOrImplements(typeof(T))) {
                    fInternalClipboard.RemoveAt(i);
                }
            }

            fInternalClipboard.Insert(0, obj);
        }

        public T GetClipboardObj<T>() where T : class
        {
            /*string objName = typeof(T).FullName;
            string objContent = AppHost.Instance.GetClipboardObject(objName);
            if (!string.IsNullOrEmpty(objContent)) {
                T result = new T();
                //GEDCOMProvider.SetTagStreamText(result, objContent);
                return result;
            }
            return null;*/

            foreach (var obj in fInternalClipboard) {
                if (obj.GetType().IsDerivedFromOrImplements(typeof(T))) {
                    return (T)obj;
                }
            }

            return null;
        }

        #endregion

        #region Executing environment

        private static Assembly GetImplementationAssembly()
        {
            Assembly asm = (fInstance != null) ? fInstance.GetType().Assembly : Assembly.GetEntryAssembly();
            if (asm == null) asm = Assembly.GetExecutingAssembly();
            return asm;
        }

        public static Version GetAppVersion()
        {
            var asm = GetImplementationAssembly();
            return (asm == null) ? null : asm.GetName().Version;
        }

        public static string GetAppCopyright()
        {
            var asm = GetImplementationAssembly();
            if (asm == null) {
                return string.Empty;
            } else {
                var attr = SysUtils.GetAssemblyAttribute<AssemblyCopyrightAttribute>(asm);
                return (attr == null) ? string.Empty : attr.Copyright;
            }
        }

        protected static string GetAppSign()
        {
            return fAppSign;
        }

        protected static void SetAppSign(string value)
        {
            fAppSign = value;
        }

        public static string GetAppPath()
        {
            return GKUtils.GetAppPath();
        }

        public static string GetAppDataPathStatic()
        {
            string path;

            if (string.IsNullOrEmpty(fAppDataPath)) {
                Environment.SpecialFolder specialFolder;
                if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                    specialFolder = Environment.SpecialFolder.ApplicationData; // for compatibility
                } else {
                    specialFolder = Environment.SpecialFolder.LocalApplicationData;
                }

                path = Environment.GetFolderPath(specialFolder) + Path.DirectorySeparatorChar + GetAppSign() + Path.DirectorySeparatorChar;
            } else {
                path = fAppDataPath;
            }

            try {
                if (!Directory.Exists(path)) Directory.CreateDirectory(path);
            } catch (Exception ex) {
                Logger.WriteError("AppHost.GetAppDataPathStatic()", ex);
            }

            return path;
        }

        public static string GetCachePath()
        {
            string path = GetAppDataPathStatic() + "imagecache" + Path.DirectorySeparatorChar;
            if (!Directory.Exists(path)) Directory.CreateDirectory(path);
            return path;
        }

        public static void CleanImagesCache()
        {
            string path = GetCachePath();

            DirectoryInfo dir = new DirectoryInfo(path);
            foreach (FileInfo fi in dir.GetFiles()) {
                try {
                    fi.Delete();
                } catch (Exception) { } // Ignore all exceptions
            }
        }

        public static string GetLogFilename()
        {
            string path = GetAppDataPathStatic() + "GEDKeeper2.log";
            return path;
        }

        #endregion

        #region IHost implementation

        public virtual void CloseDependentWindows(IWindow owner)
        {
            // May have a desktop-only implementation
        }

        public IWorkWindow GetWorkWindow()
        {
            IWindow activeWnd = GetActiveWindow();
            return (activeWnd is IWorkWindow) ? (IWorkWindow)activeWnd : null;
        }

        public virtual IBaseWindow GetCurrentFile(bool extMode = false)
        {
            IWindow activeWnd = GetActiveWindow();
            IChartWindow curChart = (activeWnd is IChartWindow) ? (IChartWindow)activeWnd : null;
            IBaseWindow result;

            if (extMode && curChart != null) {
                result = curChart.OwnerWindow as IBaseWindow;
            } else {
                result = (activeWnd is IBaseWindow) ? (IBaseWindow)activeWnd : null;
            }

            return result;
        }

        public ILangMan CreateLangMan(object sender)
        {
            return AppHost.Plugins.CreateLangMan(sender);
        }

        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            Plugins.NotifyRecord(baseWin, record, action);
        }

        public void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, IListFilter filter)
        {
            Plugins.NotifyFilter(baseWin, recType, listSource, filter);
        }

        public string GetAppDataPath()
        {
            return GetAppDataPathStatic();
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

        public abstract ExtRect GetActiveScreenWorkingArea();

        public abstract void SetWindowBounds(IWindow window, ExtRect bounds);

        public void SetWindowBounds(IWindow window, ChartWindowsShowMode mode)
        {
            var scrBounds = GetActiveScreenWorkingArea();

            switch (mode) {
                case ChartWindowsShowMode.Default:
                    break;

                case ChartWindowsShowMode.Maximize:
                    SetWindowBounds(window, scrBounds);
                    break;

                case ChartWindowsShowMode.LeftHalf: {
                        int half = scrBounds.Width / 2;
                        var halfBounds = new ExtRect(scrBounds.Left, scrBounds.Top, half, scrBounds.Height);
                        SetWindowBounds(window, halfBounds);
                    }
                    break;

                case ChartWindowsShowMode.RightHalf: {
                        int half = scrBounds.Width / 2;
                        var halfBounds = new ExtRect(scrBounds.Left + half, scrBounds.Top, half, scrBounds.Height);
                        SetWindowBounds(window, halfBounds);
                    }
                    break;
            }
        }

        public virtual void WidgetLocate(IWidgetForm view, WidgetLocation location)
        {
            // May have a desktop-only implementation
        }

        protected ExtPoint WidgetLocate(ExtRect formBounds, WidgetLocation location)
        {
            const int ScrPaddingX = 10;
            const int ScrPaddingYt = 50;
            const int ScrPaddingYb = 10;

            var screenWorkingArea = GetActiveScreenWorkingArea();

            int locX = 0;
            int locY = 0;

            if ((location & WidgetLocation.HLeft) != WidgetLocation.None) {
                locX = ScrPaddingX;
            } else if ((location & WidgetLocation.HCenter) != WidgetLocation.None) {
                locX = (screenWorkingArea.Width - formBounds.Width) / 2;
            } else if ((location & WidgetLocation.HRight) != WidgetLocation.None) {
                locX = screenWorkingArea.Width - formBounds.Width - ScrPaddingX;
            }

            if ((location & WidgetLocation.VTop) != WidgetLocation.None) {
                locY = ScrPaddingYt;
            } else if ((location & WidgetLocation.VCenter) != WidgetLocation.None) {
                locY = (screenWorkingArea.Height - formBounds.Height) / 2;
            } else if ((location & WidgetLocation.VBottom) != WidgetLocation.None) {
                locY = screenWorkingArea.Height - formBounds.Height - ScrPaddingYb;
            }

            return new ExtPoint(screenWorkingArea.Left + locX, screenWorkingArea.Top + locY);
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

        public virtual void WidgetsEnable()
        {
            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.WidgetEnable();
            }
        }

        public virtual void BaseChanged(IBaseWindow baseWin)
        {
            bool forceDeactivate = (baseWin == null);
            AppHost.Instance.UpdateControls(forceDeactivate);

            // In an SDI application, passing a null value produces an incorrect result
            if (baseWin == null) return;

            fActiveBase = baseWin;

            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.BaseChanged(baseWin);
            }
        }

        public virtual void BaseClosed(IBaseWindow baseWin)
        {
            fActiveBase = null;

            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.BaseClosed(baseWin);
            }

            SaveWinMRU(baseWin);
        }

        public virtual void BaseRenamed(IBaseWindow baseWin, string oldName, string newName)
        {
            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.BaseRenamed(baseWin, oldName, newName);
            }
        }

        public virtual void BaseSaved(IBaseWindow baseWin, string fileName)
        {
            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.BaseSaved(baseWin, fileName);
            }
        }

        public virtual void SelectedIndexChanged(IBaseWindow baseWin)
        {
            if (baseWin == null) return;

            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.SelectedIndexChanged(baseWin);
            }
        }

        public virtual void TabChanged(IBaseWindow baseWin)
        {
            if (baseWin == null) return;

            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.TabChanged(baseWin);
            }
        }

        public virtual async Task<bool> ShowModalAsync(ICommonDialog dialog, IView owner, bool keepModeless = false)
        {
            return false;
        }

        public void ShowHelpTopic(string topic)
        {
            string lngSign = AppHost.Options.GetLanguageSign();
            if (string.IsNullOrEmpty(lngSign)) return;

            if (string.IsNullOrEmpty(topic)) {
                topic = "GEDKeeper.html";
            }
            string topicWL = "help_" + lngSign + "/" + topic;

            if (!HasFeatureSupport(Feature.Mobile)) {
                string topicPath = GKUtils.GetLangsPath() + topicWL;
                if (!File.Exists(topicPath)) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.HelpIsNotAvailable));
                    topicWL = "help_enu/" + topic;
                    topicPath = GKUtils.GetLangsPath() + topicWL;
                }
                GKUtils.LoadExtFile(topicPath);
            } else {
                string topicURL = GKData.APP_SITE + topicWL;
                OpenURL(topicURL);
            }
        }

        public virtual void OpenURL(string uriString)
        {
        }

        public string GetUserFilesPath(string filePath)
        {
            string ufPath = filePath;
            if (Directory.Exists(ufPath)) return ufPath;

            ufPath = AppHost.Options.LastDir;
            if (Directory.Exists(ufPath)) return ufPath;

            ufPath = GKUtils.GetHomePath();
            return ufPath;
        }

        private void ProcessLoaded(IBaseContext context)
        {
            if (GlobalOptions.Instance.ShowTips) {
                context.CollectTips(fTips);
            }
        }

        public virtual async Task<IBaseWindow> CreateBase(string fileName)
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

                    result = AppHost.Container.Resolve<IBaseWindow>();
                    ShowWindow(result);

                    if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName)) {
                        result.LoadFile(fileName);
                        ProcessLoaded(result.Context);
                    } else {
                        result.CreateNewFile();
                    }

                    RestoreWinMRU(result);

                    await EndLoading();
                } finally {
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.CreateBase()", ex);
            }

            return result;
        }

        public async Task LoadBase(IBaseWindow baseWin, string fileName)
        {
            if (baseWin == null)
                throw new ArgumentNullException(@"baseWin");

            try {
                if (!HasFeatureSupport(Feature.Mobile)) {
                    // Multiple documents/bases
                    if (!baseWin.Context.IsUnknown() || !baseWin.Context.Tree.IsEmpty) {
                        await CreateBase(fileName);
                        return;
                    }

                    IBaseWindow result = FindBase(fileName);
                    if (result != null) {
                        result.Activate();
                        return;
                    }
                } else {
                    // Single document/base
                    if (!baseWin.CheckModified()) {
                        return;
                    }
                }

                try {
                    BeginLoading();

                    if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName)) {
                        baseWin.LoadFile(fileName);
                        ProcessLoaded(baseWin.Context);
                        RestoreWinMRU(baseWin);
                    }

                    await EndLoading();
                } finally {
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.LoadBase()", ex);
            }
        }

        public IBaseWindow FindBase(string fileName)
        {
            IBaseWindow result = null;

            foreach (var baseWin in GetRunningForms<IBaseWindow>()) {
                if (string.Equals(baseWin.Context.FileName, fileName)) {
                    result = baseWin;
                    break;
                }
            }

            return result;
        }

        public void CriticalSave()
        {
            try {
                foreach (var baseWin in GetRunningForms<IBaseWindow>()) {
                    baseWin.Context.CriticalSave();
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.CriticalSave()", ex);
            }
        }

        /// <summary>
        /// Verify and load the databases specified in the command line arguments.
        /// </summary>
        /// <returns>number of loaded files</returns>
        public async Task<int> LoadArgs(bool invoke)
        {
            int result = 0;
            if (fCommandArgs != null && fCommandArgs.Length > 0) {
                foreach (var arg in fCommandArgs) {
                    if (File.Exists(arg)) {
                        if (invoke) {
                            var baseWin = GetActiveWindow() as IBaseWindowView;
                            baseWin.LoadBase(arg);
                        } else {
                            await AppHost.Instance.CreateBase(arg);
                        }
                        result += 1;
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// Reload recently opened files at startup.
        /// </summary>
        public async Task<int> ReloadRecentBases()
        {
            int result = 0;
            if (!GlobalOptions.Instance.LoadRecentFiles) return result;

            try {
                BeginLoading();

                var lastBases = AppHost.Options.LastBases;

                int num = lastBases.Count;
                for (int i = 0; i < num; i++) {
                    string lb = lastBases[i];
                    if (File.Exists(lb)) {
                        await AppHost.Instance.CreateBase(lb);
                        result += 1;
                    }
                }

                await EndLoading();
            } finally {
            }

            return result;
        }

        public void ProcessLocales()
        {
            try {
                fLocalesCollection.Load(GKUtils.GetLangsPath() + "locales.yaml");
            } catch (Exception ex) {
                Logger.WriteError("AppHost.ProcessLocales()", ex);
            }
        }

        public void ProcessHolidays()
        {
            try {
                if (!AppHost.Options.ShowTips)
                    return;

                Holidays holidays = new Holidays();

                // TODO: We need a reference to the country, not the language
                string lngSign = AppHost.Options.GetLanguageSign();
                if (!string.IsNullOrEmpty(lngSign)) {
                    holidays.Load(GKUtils.GetLangsPath() + "holidays_" + lngSign + ".yaml");
                }

                holidays.CollectTips(fTips);
            } catch (Exception ex) {
                Logger.WriteError("AppHost.ProcessHolidays()", ex);
            }
        }

        public void ProcessTips()
        {
            try {
                if (!AppHost.Options.ShowTips)
                    return;

                var tips = new Tips();

                string lngSign = AppHost.Options.GetLanguageSign();
                if (!string.IsNullOrEmpty(lngSign)) {
                    tips.Load(GKUtils.GetLangsPath() + "tips_" + lngSign + ".yaml");
                }

                tips.CollectTips(fTips);
            } catch (Exception ex) {
                Logger.WriteError("AppHost.ProcessTips()", ex);
            }
        }

        public void UpdateControls(bool forceDeactivate, bool blockDependent = false)
        {
            foreach (var baseWin in GetRunningForms<IBaseWindow>()) {
                baseWin.UpdateControls(forceDeactivate, blockDependent);
            }
        }

        public void Restore()
        {
            // dummy
        }

        public static async Task<ushort> RequestLanguage()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                using (var dlg = AppHost.ResolveDialog<ILanguageSelectDlg>()) {
                    if (await AppHost.Instance.ShowModalAsync(dlg, null, false)) {
                        return (ushort)dlg.SelectedLanguage;
                    }
                }
            }

            return LangMan.LS_DEF_CODE;
        }

        public async Task LoadLanguage(int langCode, bool startup)
        {
            try {
                if (langCode <= 0) {
                    langCode = await RequestLanguage();
                }

                if (!startup) DoneEventDefs();

                AppHost.Options.LoadLanguage(langCode);

                InitEventDefs();

                AppHost.Plugins.OnLanguageChange();

                UpdateLang();
            } catch (Exception ex) {
                Logger.WriteError("AppHost.LoadLanguage()", ex);
            }
        }

        public void AddMRU(string fileName)
        {
            int idx = AppHost.Options.MRUFiles_IndexOf(fileName);

            MRUFile mf;
            if (idx >= 0) {
                mf = AppHost.Options.MRUFiles[idx];
                AppHost.Options.MRUFiles.RemoveAt(idx);
            } else {
                mf = new MRUFile(fileName);
            }

            AppHost.Options.MRUFiles.Insert(0, mf);

            UpdateMRU();
        }

        #region Geocoding

        public IGeocoder CreateGeocoder()
        {
            ProxyOptions proxyOptions = fOptions.Proxy;
            IWebProxy proxy = null;
            if (proxyOptions.UseProxy) {
                proxy = new WebProxy(proxyOptions.Server + ":" + proxyOptions.Port, true) {
                    Credentials = CredentialCache.DefaultCredentials
                };
            }

            IGeocoder geocoder = IGeocoder.Create(fOptions.Geocoder, fOptions.GeoSearchCountry);
            geocoder.SetProxy(proxy);

            return geocoder;
        }

        public void RequestGeoCoords(string searchValue, IList<GeoPoint> pointsList)
        {
            if (string.IsNullOrEmpty(searchValue))
                throw new ArgumentNullException(@"searchValue");

            if (pointsList == null)
                throw new ArgumentNullException(@"pointsList");

            try {
                IGeocoder geocoder = CreateGeocoder();

                IEnumerable<GeoPoint> geoPoints = geocoder.Geocode(searchValue, 1);
                foreach (GeoPoint pt in geoPoints) {
                    pointsList.Add(pt);
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.RequestGeoCoords()", ex);
            }
        }

        public static TTypeToResolve ResolveDialog<TTypeToResolve>(params object[] parameters) where TTypeToResolve : ICommonDialog
        {
            Type resolveType = typeof(TTypeToResolve);

            int num = AppHost.Plugins.Count;
            for (int i = 0; i < num; i++) {
                var dlgPlugin = AppHost.Plugins[i] as IDialogReplacement;

                if (dlgPlugin != null && dlgPlugin.Category == PluginCategory.DialogReplacement) {
                    var dlgType = dlgPlugin.GetDialogType();
                    if (SysUtils.ImplementsInterface(dlgType, resolveType) && dlgPlugin.Enabled) {
                        return (TTypeToResolve)dlgPlugin.CreateDialog(parameters);
                    }
                }
            }

            return Container.Resolve<TTypeToResolve>(parameters);
        }

        #endregion

        public void ShowOptions(IView owner)
        {
            OptionsPage page = OptionsPage.opCommon;
            IWindow activeWin = GetActiveWindow();
            if (activeWin is IBaseWindow) page = OptionsPage.opInterface;
            if (activeWin is IChartWindow) {
                if (activeWin is ICircleChartWin) {
                    page = OptionsPage.opCircleChart;
                } else {
                    page = OptionsPage.opTreeChart;
                }
            }

            ShowOptions(owner, page);
        }

        public async void ShowOptions(IView owner, OptionsPage page)
        {
            using (var dlg = AppHost.ResolveDialog<IOptionsDlg>(AppHost.Instance)) {
                dlg.SetPage(page);

                if (await AppHost.Instance.ShowModalAsync(dlg, owner)) {
                    AppHost.Instance.ApplyOptions();
                }
            }
        }

        public virtual void ApplyOptions()
        {
            double interval = AppHost.Options.AutosaveInterval /* min */ * 60 * 1000;
            if (fAutosaveTimer == null) {
                fAutosaveTimer = CreateTimer(interval, AutosaveTimer_Tick);
            } else {
                fAutosaveTimer.Interval = interval;
            }
            fAutosaveTimer.Enabled = AppHost.Options.Autosave;

            foreach (var win in GetRunningForms<IWorkWindow>()) {
                win.UpdateSettings();
            }
        }

        public void ApplyTheme(string name)
        {
            if (!HasFeatureSupport(Feature.Themes))
                return;

            ThemeManager.SetTheme(name);
            GlobalOptions.Instance.Theme = name;

            foreach (var themedView in GetRunningForms<IThemedView>()) {
                themedView.ApplyTheme();
            }
        }

        public void ApplyTheme(IThemedView view)
        {
            if (!HasFeatureSupport(Feature.Themes))
                return;

            ThemeManager.ApplyTheme(view);
        }

        public void ApplyTheme(IThemedView view, object component)
        {
            if (!HasFeatureSupport(Feature.Themes))
                return;

            ThemeManager.ApplyTheme(view, component);
        }

        public abstract string SelectFolder(string folderPath);

        public virtual bool HasFeatureSupport(Feature feature)
        {
            return false;
        }

        #endregion

        #region ISingleInstanceEnforcer implementation

        public static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
        {
            return AppHost.Instance;
        }

        void ISingleInstanceEnforcer.OnMessageReceived(MessageEventArgs e)
        {
            OnMessageReceivedInvoker invoker = delegate (MessageEventArgs eventArgs) {
                try {
                    string msg = eventArgs.Message as string;

                    if (!string.IsNullOrEmpty(msg) && msg == "restore") {
                        Restore();
                    } else {
                        string[] args = eventArgs.Message as string[];
                        if (args != null) {
                            // A obligatory recovery of window, otherwise it will fail to load
                            Restore();
                            SetArgs(args);
                            LoadArgs(true);
                        }
                    }
                } catch (Exception ex) {
                    Logger.WriteError("AppHost.OnMessageReceived()", ex);
                }
            };

            //if (InvokeRequired) {
            //    Invoke(invoker, e);
            //} else {
            invoker(e);
            //}
        }

        #endregion

        #region Static members

        private static readonly IocContainer fIocContainer;

        private static EventDefinitions fEventDefinitions;
        private static ExtResources fExtResources;
        private static IGraphicsProvider fGfxProvider;
        private static INamesTable fNamesTable;
        private static GlobalOptions fOptions;
        private static PluginsMan fPlugins;
        private static IStdDialogs fStdDialogs;
        private static IThemeManager fThemeManager;


        public static IocContainer Container
        {
            get { return fIocContainer; }
        }

        public static EventDefinitions EventDefinitions
        {
            get {
                if (fEventDefinitions == null) {
                    fEventDefinitions = new EventDefinitions();
                }
                return fEventDefinitions;
            }
        }

        public static ExtResources ExtResources
        {
            get {
                if (fExtResources == null) {
                    fExtResources = new ExtResources();
                }
                return fExtResources;
            }
        }

        public static IGraphicsProvider GfxProvider
        {
            get {
                if (fGfxProvider == null) {
                    fGfxProvider = fIocContainer.Resolve<IGraphicsProvider>();
                }
                return fGfxProvider;
            }
        }

        public static INamesTable NamesTable
        {
            get {
                if (fNamesTable == null) {
                    fNamesTable = new NamesTable();
                }
                return fNamesTable;
            }
        }

        public static GlobalOptions Options
        {
            get {
                if (fOptions == null) {
                    fOptions = GlobalOptions.Instance;
                }
                return fOptions;
            }
        }

        public static PluginsMan Plugins
        {
            get {
                if (fPlugins == null) {
                    fPlugins = new PluginsMan();
                }
                return fPlugins;
            }
        }

        public static IStdDialogs StdDialogs
        {
            get {
                if (fStdDialogs == null) {
                    fStdDialogs = fIocContainer.Resolve<IStdDialogs>();
                }
                return fStdDialogs;
            }
        }

        public static IThemeManager ThemeManager
        {
            get {
                if (fThemeManager == null) {
                    fThemeManager = fIocContainer.Resolve<IThemeManager>();
                }
                return fThemeManager;
            }
        }


        static AppHost()
        {
            AppDomain.CurrentDomain.UnhandledException += UnhandledExceptionsHandler;

            fIocContainer = new IocContainer();
        }

        private static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs e)
        {
            try {
                // Saving the copy for restoration
                AppHost.Instance.CriticalSave();
                Logger.WriteError("GK.UnhandledExceptionsHandler()", (Exception)e.ExceptionObject);
            } catch (Exception) {
            }
        }

        public static void CheckPortable(string[] args)
        {
            const string HomeDirArg = "-homedir:";
            const string LocalAppDataFolder = "appdata\\";

            string appPath = GetAppPath();

            string homedir = "";
            if (args != null && args.Length > 0) {
                foreach (var arg in args) {
                    if (arg.StartsWith(HomeDirArg)) {
                        homedir = arg.Remove(0, HomeDirArg.Length);
                    }
                }
            }

            if (!string.IsNullOrEmpty(homedir)) {
                string path = Path.Combine(appPath, homedir);
                if (Directory.Exists(path)) {
                    fAppDataPath = Path.Combine(path, LocalAppDataFolder);
                }
            }

            if (string.IsNullOrEmpty(fAppDataPath)) {
                string path = Path.Combine(appPath, LocalAppDataFolder);
                if (Directory.Exists(path)) {
                    fAppDataPath = path;
                }
            }
        }

        public static void InitSettings()
        {
            Logger.Init(GetLogFilename());

            Plugins.Load(AppHost.Instance, GKUtils.GetPluginsPath());

            var options = GlobalOptions.Instance;
            options.LoadFromFile(GetAppDataPathStatic() + "GEDKeeper2.ini");
            options.FindLanguages();

            NamesTable.LoadFromFile(GetAppDataPathStatic() + "GEDKeeper2.nms");

            ExtResources.Load(GKUtils.GetExternalsPath() + "resources.yaml");
        }

        public static void DoneSettings()
        {
            NamesTable.SaveToFile(GetAppDataPathStatic() + "GEDKeeper2.nms");

            DoneEventDefs();

            var options = GlobalOptions.Instance;
            options.SaveToFile(GetAppDataPathStatic() + "GEDKeeper2.ini");

            // In a mobile app, closing the app causes sleep rather than exit. No objects dispose is required.
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                options.Dispose();
                Plugins.Unload();
            }
        }

        private static string GetEventDefsFileName()
        {
            string lang_sign = GlobalOptions.Instance.GetLanguageSign();
            string result = string.Format(GetAppDataPathStatic() + "event_defs_{0}.yaml", lang_sign);
            return result;
        }

        private static void InitEventDefs()
        {
            EventDefinitions.Load(GetEventDefsFileName());
        }

        private static void DoneEventDefs()
        {
            EventDefinitions.Save(GetEventDefsFileName());
        }

        public static void ForceGC()
        {
            GC.Collect(2, GCCollectionMode.Forced);
            GC.WaitForPendingFinalizers();
        }

        #endregion
    }
}
