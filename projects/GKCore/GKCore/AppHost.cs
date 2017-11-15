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
using System.IO;
using System.Net;
using System.Reflection;

using GKCommon;
using GKCommon.IoC;
using GKCore.Geocoding;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.SingleInstance;
using GKCore.Types;
using GKCore.UIContracts;

// TODO: total search and fix references to Encoding.GetEncoding(1251)!
namespace GKCore
{
    internal delegate void OnMessageReceivedInvoker(MessageEventArgs e);

    public class WidgetInfo
    {
        public IWidget Widget;
        public IMenuItem MenuItem;
    }

    /// <summary>
    /// Global controller of UI for the isolation
    /// of presentation from logic and data model (based on IoC).
    /// </summary>
    public abstract class AppHost : IHost, ISingleInstanceEnforcer
    {
        private static string fAppSign;
        private static AppHost fInstance = null;

        private readonly List<WidgetInfo> fActiveWidgets;
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
                Logger.LogWrite("AppHost.AutosaveTimer_Tick(): " + ex.Message);
            }
        }

        public virtual void Init(string[] args, bool isMDI)
        {
            LoadLanguage(AppHost.Options.InterfaceLang);
            SetArgs(args);
            StartupWork();

            //LangMan.SaveDefaultLanguage();
        }

        public void StartupWork()
        {
            try
            {
                ApplyOptions();

                try
                {
                    BeginLoading();

                    int result = LoadArgs();
                    result += ReloadRecentBases();
                    if (result == 0) {
                        CreateBase("");
                    }

                    ProcessHolidays();
                } finally {
                    EndLoading();
                }

                UpdateMan.CheckUpdate();
            } catch (Exception ex) {
                Logger.LogWrite("AppHost.StartupWork(): " + ex.Message);
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

            using (var dlg = fIocContainer.Resolve<IDayTipsDlg>())
            {
                dlg.Init(LangMan.LS(LSID.LSID_BirthDays), AppHost.Options.ShowTips, fTips);
                ShowModalX(dlg, false);
                AppHost.Options.ShowTips = dlg.ShowTipsChecked;
            }

            fTips.Clear();
        }

        public abstract IWindow GetActiveWindow();

        public abstract IntPtr GetTopWindowHandle();

        public abstract void ShowWindow(IWindow window);

        public abstract void EnableWindow(IWidgetForm form, bool value);

        public abstract void SaveWinMRU(IBaseWindow baseWin);

        public abstract void RestoreWinMRU(IBaseWindow baseWin);

        protected abstract void UpdateLang();

        protected abstract void UpdateMRU();

        public abstract void SaveLastBases();

        public abstract int GetKeyLayout();

        public abstract void SetKeyLayout(int layout);

        public abstract ITimer CreateTimer(double msInterval, EventHandler elapsedHandler);

        #region Executing environment

        public abstract Assembly GetExecutingAssembly();

        public Version GetAppVersion()
        {
            return GetExecutingAssembly().GetName().Version;
        }

        public string GetAppCopyright()
        {
            var attr = SysUtils.GetAssemblyAttribute<AssemblyCopyrightAttribute>(GetExecutingAssembly());
            return (attr == null) ? string.Empty : attr.Copyright;
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
            string path = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) +
                Path.DirectorySeparatorChar + GetAppSign() + Path.DirectorySeparatorChar;
            if (!Directory.Exists(path)) Directory.CreateDirectory(path);
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

        public IWorkWindow GetWorkWindow()
        {
            IWindow activeWnd = GetActiveWindow();
            return (activeWnd is IWorkWindow) ? (IWorkWindow) activeWnd : null;
        }

        public IBaseWindow GetCurrentFile(bool extMode = false)
        {
            IWindow activeWnd = GetActiveWindow();
            IChartWindow curChart = ((activeWnd is IChartWindow) ? ((IChartWindow) activeWnd) : null);
            IBaseWindow result;

            if (extMode && curChart != null) {
                result = curChart.Base;
            } else {
                result = ((activeWnd is IBaseWindow) ? ((IBaseWindow) activeWnd) : null);
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

        public bool IsWidgetActive(IWidget widget)
        {
            WidgetInfo widInfo = FindWidgetInfo(widget);
            if (widInfo == null || widInfo.MenuItem == null) {
                return false;
            } else {
                return widInfo.MenuItem.Checked;
            }
        }

        public void WidgetsEnable()
        {
            foreach (WidgetInfo widgetInfo in fActiveWidgets) {
                widgetInfo.Widget.WidgetEnable();
            }
        }

        public void BaseChanged(IBaseWindow baseWin)
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
        }

        public void BaseRenamed(IBaseWindow baseWin, string oldName, string newName)
        {
            // TODO: implementation of Base.SaveAs
        }

        public virtual bool ShowModalX(ICommonDialog form, bool keepModeless = false)
        {
            var owner = GetActiveWindow();
            return (form == null) ? false : form.ShowModalX(owner);
        }

        public void ShowHelpTopic(string topic)
        {
            string lngSign = AppHost.Options.GetLanguageSign();
            if (string.IsNullOrEmpty(lngSign)) return;

            string helpPath = GKUtils.GetHelpPath(lngSign);

            if (string.IsNullOrEmpty(topic)) {
                topic = helpPath + "GEDKeeper2.html";
            } else {
                topic = helpPath + topic;
            }

            if (!File.Exists(topic)) {
                AppHost.StdDialogs.ShowError(@"For that language help is unavailable");
                return;
            }

            SysUtils.LoadExtFile(topic);
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
                        result.Context.CollectTips(fTips);
                    } else {
                        result.CreateNewFile();
                    }

                    RestoreWinMRU(result);
                } finally {
                    EndLoading();
                }
            } catch (Exception ex) {
                Logger.LogWrite("AppHost.CreateBase(): " + ex.Message);
            }

            return null;
        }

        public void LoadBase(IBaseWindow baseWin, string fileName)
        {
            if (baseWin == null)
                throw new ArgumentNullException(@"baseWin");

            try {
                if (!baseWin.Context.IsUnknown() || !baseWin.Context.Tree.IsEmpty) {
                    CreateBase(fileName);
                    return;
                }

                try {
                    BeginLoading();

                    IBaseWindow result = FindBase(fileName);
                    if (result != null) {
                        result.Activate();
                        return;
                    }

                    if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName)) {
                        baseWin.LoadFile(fileName);
                        baseWin.Context.CollectTips(fTips);
                        RestoreWinMRU(baseWin);
                    }
                } finally {
                    EndLoading();
                }
            } catch (Exception ex) {
                Logger.LogWrite("AppHost.LoadBase(): " + ex.Message);
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
                Logger.LogWrite("AppHost.CriticalSave(): " + ex.Message);
            }
        }

        public int LoadArgs()
        {
            int result = 0;
            if (fCommandArgs != null && fCommandArgs.Length > 0) {
                AppHost.Instance.CreateBase(fCommandArgs[0]);
                result += 1;
            }
            return result;
        }

        /// <summary>
        /// Reload at startup recent opened files.
        /// </summary>
        public int ReloadRecentBases()
        {
            int result = 0;
            if (!GlobalOptions.Instance.LoadRecentFiles) return result;

            try {
                BeginLoading();

                int num = AppHost.Options.GetLastBasesCount();
                for (int i = 0; i < num; i++) {
                    string lb = AppHost.Options.GetLastBase(i);
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
            try
            {
                if (!AppHost.Options.ShowTips) return;

                Holidays holidays = new Holidays();

                // TODO: We need a reference to the country, not the language
                string lngSign = AppHost.Options.GetLanguageSign();
                if (!string.IsNullOrEmpty(lngSign)) {
                    holidays.Load(GKUtils.GetLangsPath() + "holidays_" + lngSign + ".yaml");
                }

                holidays.CollectTips(fTips);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("AppHost.ProcessHolidays(): " + ex.Message);
            }
        }

        // FIXME: rework this
        public void UpdateControls(bool forceDeactivate)
        {
            int num = fRunningForms.Count;
            for (int i = 0; i < num; i++) {
                IBaseWindow baseWin = fRunningForms[i] as IBaseWindow;
                if (baseWin != null) {
                    baseWin.UpdateControls(forceDeactivate);
                }
            }
        }

        public void Restore()
        {
            // FIXME!
        }

        private static ushort RequestLanguage()
        {
            using (var dlg = AppHost.Container.Resolve<ILanguageSelectDlg>()) {
                if (AppHost.Instance.ShowModalX(dlg, false)) {
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

                    foreach (LangRecord langRec in AppHost.Options.Languages) {
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

                AppHost.Options.InterfaceLang = (ushort)langCode;
                AppHost.Plugins.OnLanguageChange();

                UpdateLang();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("AppHost.LoadLanguage(): " + ex.Message);
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

        public abstract string GetDefaultFontName();

        #region Geocoding

        public IGeocoder CreateGeocoder()
        {
            ProxyOptions proxyOptions = fOptions.Proxy;
            IWebProxy proxy = null;
            if (proxyOptions.UseProxy)
            {
                proxy = new WebProxy(proxyOptions.Server + ":" + proxyOptions.Port, true)
                {
                    Credentials = CredentialCache.DefaultCredentials
                };
            }

            IGeocoder geocoder = IGeocoder.Create(fOptions.Geocoder);
            geocoder.SetKey(GKData.GAPI_KEY);
            geocoder.SetProxy(proxy);

            return geocoder;
        }

        public void RequestGeoCoords(string searchValue, IList<GeoPoint> pointsList)
        {
            if (string.IsNullOrEmpty(searchValue))
                throw new ArgumentNullException(@"searchValue");

            if (pointsList == null)
                throw new ArgumentNullException(@"pointsList");

            try
            {
                IGeocoder geocoder = CreateGeocoder();

                IEnumerable<GeoPoint> geoPoints = geocoder.Geocode(searchValue, 1);
                foreach (GeoPoint pt in geoPoints)
                {
                    pointsList.Add(pt);
                }
            } catch (Exception ex) {
                Logger.LogWrite("AppHost.RequestGeoCoords(): " + ex.Message);
            }
        }

        #endregion

        public virtual void ApplyOptions()
        {
            double interval = AppHost.Options.AutosaveInterval /* min */ * 60 * 1000;

            if (fAutosaveTimer == null) {
                fAutosaveTimer = CreateTimer(interval, AutosaveTimer_Tick);
            } else {
                fAutosaveTimer.Interval = interval;
            }
            fAutosaveTimer.Enabled = AppHost.Options.Autosave;
        }

        #endregion

        #region ISingleInstanceEnforcer implementation

        // FIXME!
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
                    Logger.LogWrite("AppHost.OnMessageReceived(): " + ex.Message);
                }
            };

            //if (InvokeRequired) {
            //    Invoke(invoker, e);
            //} else {
            invoker(e);
            //}
        }

        void ISingleInstanceEnforcer.OnNewInstanceCreated(EventArgs e)
        {
        }

        #endregion

        #region Static members

        private static readonly IocContainer fIocContainer;

        private static IStdDialogs fStdDialogs;
        private static IGraphicsProvider fGfxProvider;
        private static PathReplacer fPathReplacer;
        private static INamesTable fNamesTable;
        private static IProgressController fProgressController;
        private static PluginsMan fPlugins;
        private static GlobalOptions fOptions;


        public static IocContainer Container
        {
            get { return fIocContainer; }
        }

        public static PathReplacer PathReplacer
        {
            get {
                if (fPathReplacer == null) {
                    fPathReplacer = new PathReplacer();
                }
                return fPathReplacer;
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

        public static PluginsMan Plugins
        {
            get {
                if (fPlugins == null) {
                    fPlugins = new PluginsMan();
                }
                return fPlugins;
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

        public static IProgressController Progress
        {
            get {
                if (fProgressController == null) {
                    fProgressController = fIocContainer.Resolve<IProgressController>();
                }
                return fProgressController;
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

        public static IGraphicsProvider GfxProvider
        {
            get {
                if (fGfxProvider == null) {
                    fGfxProvider = fIocContainer.Resolve<IGraphicsProvider>();
                }
                return fGfxProvider;
            }
        }


        static AppHost()
        {
            fIocContainer = new IocContainer();
        }

        public static void InitSettings()
        {
            Logger.LogInit(GetLogFilename());

            var options = GlobalOptions.Instance;
            options.LoadFromFile(GetAppDataPathStatic() + "GEDKeeper2.ini");
            options.FindLanguages();

            NamesTable.LoadFromFile(GetAppDataPathStatic() + "GEDKeeper2.nms");

            PathReplacer.Load(GKUtils.GetAppPath() + "crossplatform.yaml"); // FIXME: path

            Plugins.Load(AppHost.Instance, GKUtils.GetPluginsPath());
        }

        public static void DoneSettings()
        {
            Plugins.Unload();

            NamesTable.SaveToFile(GetAppDataPathStatic() + "GEDKeeper2.nms");

            var options = GlobalOptions.Instance;
            options.SaveToFile(GetAppDataPathStatic() + "GEDKeeper2.ini");
            options.Dispose();
        }

        #endregion
    }
}
