/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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


        public static AppHost Instance
        {
            get { return fInstance; }
        }


        public IList<WidgetInfo> ActiveWidgets
        {
            get { return fActiveWidgets; }
        }

        public IList<IWindow> RunningForms
        {
            get { return fRunningForms; }
        }


        protected AppHost()
        {
            fInstance = this;

            fActiveWidgets = new List<WidgetInfo>();
            fInternalClipboard = new List<object>();
            fRunningForms = new List<IWindow>();
            fTips = new StringList();
        }

        private void AutosaveTimer_Tick(object sender, EventArgs e)
        {
            try {
                foreach (IWindow win in fRunningForms) {
                    var baseWin = win as IBaseWindow;
                    if (baseWin != null) {
                        baseWin.CheckAutosave();
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.AutosaveTimer_Tick()", ex);
            }
        }

        public static void LogSysInfo()
        {
            try {
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

        public virtual void Init(string[] args, bool isMDI)
        {
            GKUtils.InitGEDCOM();
            LoadLanguage(AppHost.Options.InterfaceLang);
            SetArgs(args);
            StartupWork();
        }

        public virtual void StartupWork()
        {
            if (HasFeatureSupport(Feature.Themes)) {
                ThemeManager.LoadThemes();
                ThemeManager.SetTheme(GlobalOptions.Instance.Theme);
            }

            try {
                ApplyOptions();

                try {
                    BeginLoading();

                    int result = LoadArgs(false);
                    result += ReloadRecentBases();
                    if (result == 0) {
                        CreateBase("");
                    }

                    ProcessHolidays();
                    ProcessTips();
                } finally {
                    EndLoading();
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

            if (fRunningForms.Count == 0) {
                Quit();
            }
        }

        public string GetCurrentFileName()
        {
            IBaseWindow cb = GetCurrentFile();
            string result = ((cb == null) ? "" : cb.Context.FileName);
            return result;
        }

        public void BeginLoading()
        {
            fLoadingCount++;
        }

        public void EndLoading()
        {
            fLoadingCount--;

            if (fLoadingCount == 0) {
                ShowTips();
            }
        }

        public void ShowTips()
        {
            if (fTips.Count <= 0) return;

            using (var dlg = fIocContainer.Resolve<IDayTipsDlg>()) {
                dlg.Init(LangMan.LS(LSID.BirthDays), AppHost.Options.ShowTips, fTips);
                ShowModalX(dlg, null, false);
                AppHost.Options.ShowTips = dlg.ShowTipsChecked;
            }

            fTips.Clear();
        }

        public abstract void Activate();

        public abstract IForm GetActiveForm();

        public abstract IWindow GetActiveWindow();

        public abstract IntPtr GetTopWindowHandle();

        public void ShowWindow(IWindow window)
        {
            if (window != null) {
                window.Show(true);
            }
        }

        public abstract void EnableWindow(IWidgetForm form, bool value);

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

        public abstract void SaveWinState(IBaseWindow baseWin, MRUFile mf);

        public abstract void RestoreWinState(IBaseWindow baseWin, MRUFile mf);

        public void RestoreWinMRU(IBaseWindow baseWin)
        {
            var mf = GetMRUFile(baseWin);
            if (mf != null) RestoreWinState(baseWin, mf);
        }

        protected void UpdateLang()
        {
            foreach (IWindow win in fRunningForms) {
                win.SetLocale();
            }
        }

        protected void UpdateMRU()
        {
            foreach (IWindow win in fRunningForms) {
                if (win is IBaseWindow) {
                    ((IBaseWindow)win).UpdateMRU();
                }
            }
        }

        public void SaveLastBases()
        {
            #if !CI_MODE
            AppHost.Options.LastBases.Clear();

            foreach (IWindow win in fRunningForms) {
                var baseWin = win as IBaseWindow;
                if (baseWin != null) {
                    AppHost.Options.LastBases.Add(baseWin.Context.FileName);
                }
            }
            #endif
        }

        public abstract int GetKeyLayout();

        public abstract void SetKeyLayout(int layout);

        public abstract ITimer CreateTimer(double msInterval, EventHandler elapsedHandler);

        public abstract void Quit();

        public abstract void ExecuteWork(ProgressStart proc);

        public abstract bool ExecuteWorkExt(ProgressStart proc, string title);

        public abstract void LayoutWindows(WinLayout layout);

        #region Extended clipboard functions

        public abstract void SetClipboardText(string text);

        //public abstract void SetClipboardObject(string objTypeName, string objContent);

        //public abstract string GetClipboardObject(string objTypeName);

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

        private static Assembly GetAssembly()
        {
            Assembly asm = Assembly.GetEntryAssembly();
            if (asm == null) {
                asm = Assembly.GetExecutingAssembly();
            }
            return asm;
        }

        public static Version GetAppVersion()
        {
            var asm = GetAssembly();
            return (asm == null) ? null : asm.GetName().Version;
        }

        public static string GetAppCopyright()
        {
            var asm = GetAssembly();
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

        public static string GetLogFilename()
        {
            string path = GetAppDataPathStatic() + "GEDKeeper2.log";
            return path;
        }

        #endregion

        #region IHost implementation

        public abstract void CloseDependentWindows(IWindow owner);

        public IWorkWindow GetWorkWindow()
        {
            IWindow activeWnd = GetActiveWindow();
            return (activeWnd is IWorkWindow) ? (IWorkWindow) activeWnd : null;
        }

        public IBaseWindow GetCurrentFile(bool extMode = false)
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

        public abstract void WidgetLocate(IWidgetForm view, WidgetLocation location);

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

        public virtual bool ShowModalX(ICommonDialog dialog, IView owner, bool keepModeless = false)
        {
            return (dialog != null && dialog.ShowModalX(owner));
        }

        public void ShowHelpTopic(string topic)
        {
            string lngSign = AppHost.Options.GetLanguageSign();
            if (string.IsNullOrEmpty(lngSign)) return;

            string helpPath = GKUtils.GetHelpPath(lngSign);

            if (string.IsNullOrEmpty(topic)) {
                topic = helpPath + "GEDKeeper.html";
            } else {
                topic = helpPath + topic;
            }

            if (!File.Exists(topic)) {
                AppHost.StdDialogs.ShowError(@"For that language help is unavailable");
                return;
            }

            GKUtils.LoadExtFile(topic);
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

                    result = AppHost.Container.Resolve<IBaseWindow>();
                    ShowWindow(result);

                    if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName)) {
                        result.LoadFile(fileName);
                        ProcessLoaded(result.Context);
                    } else {
                        result.CreateNewFile();
                    }

                    RestoreWinMRU(result);
                } finally {
                    EndLoading();
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.CreateBase()", ex);
            }

            return result;
        }

        public void LoadBase(IBaseWindow baseWin, string fileName)
        {
            if (baseWin == null)
                throw new ArgumentNullException(@"baseWin");

            try {
                if (!HasFeatureSupport(Feature.Mobile)) {
                    // Multiple documents/bases
                    if (!baseWin.Context.IsUnknown() || !baseWin.Context.Tree.IsEmpty) {
                        CreateBase(fileName);
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
                } finally {
                    EndLoading();
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.LoadBase()", ex);
            }
        }

        public IBaseWindow FindBase(string fileName)
        {
            IBaseWindow result = null;

            int num = fRunningForms.Count;
            for (int i = 0; i < num; i++) {
                IBaseWindow baseWin = fRunningForms[i] as IBaseWindow;

                if (baseWin != null && string.Equals(baseWin.Context.FileName, fileName)) {
                    result = baseWin;
                    break;
                }
            }

            return result;
        }

        public void CriticalSave()
        {
            try
            {
                int num = fRunningForms.Count;
                for (int i = 0; i < num; i++) {
                    IBaseWindow baseWin = fRunningForms[i] as IBaseWindow;
                    if (baseWin != null) {
                        baseWin.Context.CriticalSave();
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("AppHost.CriticalSave()", ex);
            }
        }

        /// <summary>
        /// Verify and load the databases specified in the command line arguments.
        /// </summary>
        /// <returns>number of loaded files</returns>
        public int LoadArgs(bool invoke)
        {
            int result = 0;
            if (fCommandArgs != null && fCommandArgs.Length > 0) {
                foreach (var arg in fCommandArgs) {
                    if (File.Exists(arg)) {
                        if (invoke) {
                            var baseWin = GetActiveWindow() as IBaseWindowView;
                            baseWin.LoadBase(arg);
                        } else {
                            AppHost.Instance.CreateBase(arg);
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
        public int ReloadRecentBases()
        {
            int result = 0;
            if (!GlobalOptions.Instance.LoadRecentFiles) return result;

            try {
                BeginLoading();

                int num = AppHost.Options.LastBases.Count;
                for (int i = 0; i < num; i++) {
                    string lb = AppHost.Options.LastBases[i];
                    if (File.Exists(lb)) {
                        AppHost.Instance.CreateBase(lb);
                        result += 1;
                    }
                }
            } finally {
                EndLoading();
            }

            return result;
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
            int num = fRunningForms.Count;
            for (int i = 0; i < num; i++) {
                IBaseWindow baseWin = fRunningForms[i] as IBaseWindow;
                if (baseWin != null) {
                    baseWin.UpdateControls(forceDeactivate, blockDependent);
                }
            }
        }

        public void Restore()
        {
            // dummy
        }

        public static ushort RequestLanguage()
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                return LangMan.LS_DEF_CODE;
            }

            using (var dlg = AppHost.ResolveDialog<ILanguageSelectDlg>()) {
                if (AppHost.Instance.ShowModalX(dlg, null, false)) {
                    return (ushort)dlg.SelectedLanguage;
                }
            }

            return LangMan.LS_DEF_CODE;
        }

        public void LoadLanguage(int langCode)
        {
            try {
                if (langCode <= 0) {
                    langCode = RequestLanguage();
                }

                AppHost.Options.LoadLanguage(langCode);
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

        public void ShowOptions(IView owner, OptionsPage page)
        {
            using (var dlgOptions = AppHost.ResolveDialog<IOptionsDlg>(AppHost.Instance)) {
                dlgOptions.SetPage(page);

                if (AppHost.Instance.ShowModalX(dlgOptions, owner)) {
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

            foreach (IWindow win in fRunningForms) {
                if (win is IWorkWindow) {
                    (win as IWorkWindow).UpdateSettings();
                }
            }
        }

        public void ApplyTheme(string name)
        {
            if (!HasFeatureSupport(Feature.Themes))
                return;

            ThemeManager.SetTheme(name);
            GlobalOptions.Instance.Theme = name;

            foreach (IWindow win in fRunningForms) {
                IThemedView themedView = win as IThemedView;
                if (themedView != null) {
                    themedView.ApplyTheme();
                }
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
            OnMessageReceivedInvoker invoker = delegate(MessageEventArgs eventArgs) {
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

            var options = GlobalOptions.Instance;
            options.SaveToFile(GetAppDataPathStatic() + "GEDKeeper2.ini");

            // In a mobile app, closing the app causes sleep rather than exit. No objects dispose is required.
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                options.Dispose();
                Plugins.Unload();
            }
        }

        public static void ForceGC()
        {
            GC.Collect(2, GCCollectionMode.Forced);
            GC.WaitForPendingFinalizers();
        }

        #endregion
    }
}
