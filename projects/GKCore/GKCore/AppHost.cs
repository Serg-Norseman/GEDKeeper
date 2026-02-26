/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
using GKCore.Events;
using GKCore.ExtData;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Maps;
using GKCore.Names;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.Utilities;
using GKUI.Themes;

namespace GKCore
{
    internal delegate void OnMessageReceivedInvoker(MessageEventArgs e);


    /// <summary>
    /// Layout type for window arrangement.
    /// </summary>
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
        private readonly LocalesCollection fLocalesCollection;
        private readonly StringList fTips;

        protected IBaseWindow fActiveBase;
        protected IList<IWindow> fRunningForms;

        private ITimer fAutosaveTimer;
        private string[] fCommandArgs;
        private int fLoadingCount;


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

        /// <summary>
        /// Basic actions that must be performed when exiting the application.
        /// </summary>
        protected virtual void ApplicationExit()
        {
            AppHost.Instance.SaveLastBases();
            AppHost.DoneSettings();

            ControlsManager.ClearHandlerTypes();
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

        /// <summary>
        /// Logging basic system and runtime information.
        /// </summary>
        public static void LogSysInfo()
        {
            try {
                Logger.WriteInfo("OS Version: " + SysUtils.GetOSType().ToString());

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

        /// <summary>
        /// Basic actions that must be performed when launching the application.
        /// </summary>
        public virtual async Task Init(string[] args, bool isMDI)
        {
            GKUtils.InitGEDCOM();
            await LoadLanguage(AppHost.Options.InterfaceLang, true);
            SetArgs(args);

            if (HasFeatureSupport(Feature.Themes)) {
                ThemeManager.LoadThemes();
                ApplyTheme(GlobalOptions.Instance.Theme);
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

                SysUtils.LogSystemInfo();
            } catch (Exception ex) {
                Logger.WriteError("AppHost.Init()", ex);
            }
        }

        /// <summary>
        /// Setting arguments during normal application startup
        /// or via IPC transfer from a second instance.
        /// </summary>
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

        /// <summary>
        /// Display tips and reminders after loading files.
        /// </summary>
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

        /// <summary>
        /// Force activate the application window (or top work window).
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void Activate()
        {
        }

        /// <summary>
        /// Not for command-line interface (CLI).
        /// </summary>
        public virtual IForm GetActiveForm()
        {
            return null;
        }

        /// <summary>
        /// Not for command-line interface (CLI).
        /// </summary>
        public virtual IWindow GetActiveWindow()
        {
            return null;
        }

        public void ShowWindow(IWindow window)
        {
            if (window != null) {
                window.Show(true);
            }
        }

        /// <summary>
        /// Force activate the widget window.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void EnableWindow(IWidgetForm form, bool value)
        {
        }

        #region MRU support

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

        public void RestoreWinMRU(IBaseWindow baseWin)
        {
            var mf = GetMRUFile(baseWin);
            if (mf != null) RestoreWinState(baseWin, mf);
        }

        protected void UpdateMRU()
        {
            foreach (var baseWin in GetRunningForms<IBaseWindow>()) {
                baseWin.UpdateMRU();
            }
        }

        #endregion

        /// <summary>
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void SaveWinState(IBaseWindow baseWin, MRUFile mf)
        {
        }

        /// <summary>
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void RestoreWinState(IBaseWindow baseWin, MRUFile mf)
        {
        }

        /// <summary>
        /// Update the locale setting in the application and in all open windows.
        /// </summary>
        protected virtual void UpdateLang()
        {
            foreach (var win in GetRunningForms<IWindow>()) {
                win.SetLocale();
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

        /// <summary>
        /// In the future - to be able to replace the current date/time in tests and other cases.
        /// </summary>
        public DateTime GetCurrentDateTime()
        {
            return DateTime.Now;
        }

        /// <summary>
        /// Get the input language code. Not supported in the terminal.
        /// </summary>
        public virtual int GetKeyLayout()
        {
            return 0;
        }

        /// <summary>
        /// Set the input language code. Not supported in the terminal.
        /// </summary>
        public virtual void SetKeyLayout(int layout)
        {
        }

        public abstract ITimer CreateTimer(double msInterval, EventHandler elapsedHandler);

        /// <summary>
        /// Platform-dependent call to terminate the application.
        /// </summary>
        public abstract void Quit();

        public abstract bool ExecuteWork(ProgressStart proc, string title = "");

        /// <summary>
        /// Arrange windows in a multi-window desktop UI application according to a given layout.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void LayoutWindows(WinLayout layout)
        {
        }

        public virtual string GetExternalStorageDirectory()
        {
            // May have a mobile-only implementation
            throw new NotImplementedException();
        }

        #region Extended clipboard functions

        /// <summary>
        /// Copy the text to the clipboard.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void SetClipboardText(string text)
        {
        }

        /// <summary>
        /// Copy the diagram image to the clipboard.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void SetClipboardImage(object image)
        {
        }

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

        /// <summary>
        /// Set the application signature for functions that obtain system-dependent paths
        /// to application data in the user profile.
        /// </summary>
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

                path = Environment.GetFolderPath(specialFolder) + Path.DirectorySeparatorChar + fAppSign + Path.DirectorySeparatorChar;
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

        private static string GetEventDefsFileName()
        {
            string lang_sign = GlobalOptions.Instance.GetLanguageSign();
            string result = string.Format(GetAppDataPathStatic() + "event_defs_{0}.yaml", lang_sign);
            return result;
        }

        public static string GetKinshipsCultureFileName()
        {
            string lang_sign = GlobalOptions.Instance.GetLanguageSign();
            string fileName = Path.Combine(GKUtils.GetExternalsPath(), $"kinships.{lang_sign}.yaml");
            if (!File.Exists(fileName)) {
                fileName = Path.Combine(GKUtils.GetExternalsPath(), $"kinships.yaml");
            }
            return fileName;
        }

        public static string GetMapsCachePath()
        {
            string path = GetAppDataPathStatic() + "maps" + Path.DirectorySeparatorChar;
            if (!Directory.Exists(path)) Directory.CreateDirectory(path);
            return path;
        }

        #endregion

        #region IHost implementation

        /// <summary>
        /// When a database or diagram window is closed,
        /// identifies and closes all dependent auxiliary windows.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void CloseDependentWindows(IWindow owner)
        {
        }

        public IWorkWindow GetWorkWindow()
        {
            IWindow activeWnd = GetActiveWindow();
            return (activeWnd is IWorkWindow workWin) ? workWin : null;
        }

        public virtual IBaseWindow GetCurrentFile(bool extMode = false)
        {
            IWindow activeWnd = GetActiveWindow();
            IChartWindow curChart = (activeWnd is IChartWindow chartWin) ? chartWin : null;
            IBaseWindow result;

            if (extMode && curChart != null) {
                result = curChart.OwnerWindow as IBaseWindow;
            } else {
                result = (activeWnd is IBaseWindow baseWin) ? baseWin : null;
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

        public void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, ListFilter filter)
        {
            Plugins.NotifyFilter(baseWin, recType, listSource, filter);
        }

        public string GetAppDataPath()
        {
            return GetAppDataPathStatic();
        }

        private WidgetInfo FindWidgetInfo(IWidgetPlugin widget)
        {
            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                if (widgetInfo.Widget == widget) {
                    return widgetInfo;
                }
            }

            return null;
        }

        public void WidgetShow(IWidgetPlugin widget)
        {
            WidgetInfo widInfo = FindWidgetInfo(widget);
            if (widInfo == null) return;

            if (widInfo.MenuItem != null) widInfo.MenuItem.Checked = true;
        }

        public void WidgetClose(IWidgetPlugin widget)
        {
            WidgetInfo widInfo = FindWidgetInfo(widget);
            if (widInfo == null) return;

            if (widInfo.MenuItem != null) widInfo.MenuItem.Checked = false;
        }

        /// <summary>
        /// Obtaining the working area of ​​the active system screen for the chart and widget window positioning functions.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual ExtRect GetActiveScreenWorkingArea()
        {
            return ExtRect.Empty;
        }

        /// <summary>
        /// Set the position of the window according to the specified bounds.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void SetWindowBounds(IWindow window, ExtRect bounds)
        {
        }

        /// <summary>
        /// Set the position of the diagram window according to the specified setting.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
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

        /// <summary>
        /// Arrange widget window in a multi-window desktop UI application according to a given alignment type.
        /// Desktop UI only, no mobile or terminal.
        /// </summary>
        public virtual void WidgetLocate(IWidgetForm view, WidgetLocation location)
        {
        }

        /// <summary>
        /// Calculates the position of the widget (plugin) window for a given alignment type on the screen.
        /// </summary>
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

        public bool IsWidgetActive(IWidgetPlugin widget)
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
            return await Task.FromResult(TEST_MODE);
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

        private void ProcessLoaded(BaseContext context)
        {
            var globOpts = GlobalOptions.Instance;
            if (globOpts.ShowTips) {
                TipsCollector.Collect(context, fTips, globOpts.ShowTipsOnlyAlive);
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
                throw new ArgumentNullException(nameof(baseWin));

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

        /// <summary>
        /// Emergency saving in case of application crashes.
        /// </summary>
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

        public void RequestGeoCoords(string searchValue, IList<GeoPoint> pointsList, short results = 1)
        {
            if (string.IsNullOrEmpty(searchValue))
                throw new ArgumentNullException(nameof(searchValue));

            if (pointsList == null)
                throw new ArgumentNullException(nameof(pointsList));

            try {
                IGeocoder geocoder = CreateGeocoder();

                IEnumerable<GeoPoint> geoPoints = geocoder.Geocode(searchValue, results);
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
            var backupOpts = GlobalOptions.Instance.Backups;

            double interval = backupOpts.AutosaveInterval /* min */ * 60 * 1000;
            if (fAutosaveTimer == null) {
                fAutosaveTimer = CreateTimer(interval, AutosaveTimer_Tick);
            } else {
                fAutosaveTimer.Interval = interval;
            }
            fAutosaveTimer.Enabled = backupOpts.Autosave;

            foreach (var win in GetRunningForms<IWorkWindow>()) {
                win.UpdateSettings();
            }
        }

        /// <summary>
        /// Framework-specific presentation settings.
        /// For example, in GKv3, fixes to the appearance of WPF ToolBars.
        /// </summary>
        protected virtual void ApplyThemeProperties()
        {
            // dummy
        }

        public void ApplyTheme(string name)
        {
            if (!HasFeatureSupport(Feature.Themes) || !ThemeManager.SetTheme(name)) return;

            GlobalOptions.Instance.Theme = name;

            ApplyThemeProperties();

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

        public virtual bool HasFeatureSupport(Feature feature)
        {
            return false;
        }

        public abstract void Invoke(Action action);

        #endregion

        #region ISingleInstanceEnforcer implementation

        public static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
        {
            return AppHost.Instance;
        }

        void ISingleInstanceEnforcer.OnMessageReceived(MessageEventArgs e)
        {
            Invoke(delegate () {
                try {
                    string msg = e.Message as string;

                    if (!string.IsNullOrEmpty(msg) && msg == "restore") {
                        Restore();
                    } else {
                        string[] args = e.Message as string[];
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
            });
        }

        #endregion

        #region Static members

        private static readonly IocContainer fIocContainer;

        private static EventDefinitions fEventDefinitions;
        private static ExtResources fExtResources;
        private static IGraphicsProvider fGfxProvider;
        private static NamesTable fNamesTable;
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

        public static NamesTable NamesTable
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
                    fThemeManager = fIocContainer.TryResolve<IThemeManager>();
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
