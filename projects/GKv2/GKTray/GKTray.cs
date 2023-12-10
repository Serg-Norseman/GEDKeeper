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
using System.Drawing;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using BSLib;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.IoC;
using GKCore.Options;
using GKUI.Components;
using GKUI.Platform;

[assembly: AssemblyTitle("GKTray")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct(GKData.APP_TITLE)]
[assembly: AssemblyCopyright(GKData.APP_COPYRIGHT)]
[assembly: AssemblyVersion(GKData.APP_VERSION_2X)]
[assembly: AssemblyCulture("")]

namespace GKTray
{
    public sealed class GKTray
    {
        public const string APP_TITLE = "GEDKeeper Tray";

        #if !MONO

        private class TipInfo
        {
            public DateTime LastTime;
        }

        private MenuItem fAutorunItem;
        private readonly ContextMenu fMenu;
        private readonly List<MRUFile> fMRUFiles;
        private readonly NotifyIcon fNotifyIcon;
        private MenuItem fRecentFiles;
        private readonly StringList fTipsList;
        private readonly System.Threading.Timer fTipsTimer;

        public GKTray()
        {
            // for GlobalOptions initialization
            AppHost.Container.Register<IGraphicsProvider, WFGfxProvider>(LifeCycle.Singleton);

            fMRUFiles = new List<MRUFile>();

            fMenu = new ContextMenu(InitializeMenu());
            fAutorunItem.Checked = UIHelper.IsStartupItem();

            fNotifyIcon = new NotifyIcon();
            fNotifyIcon.DoubleClick += Icon_DoubleClick;
            fNotifyIcon.Icon = new Icon(GKUtils.LoadResourceStream("Resources.icon_gedkeeper.ico"));
            fNotifyIcon.ContextMenu = fMenu;

            fTipsList = new StringList();

            LoadSettings();
            LoadEvents();

            fTipsTimer = new System.Threading.Timer(this.TimerCallback, null, 1000, 1000 * 60);
        }

        private MenuItem[] InitializeMenu()
        {
            var appItem = new MenuItem("GEDKeeper", Icon_DoubleClick);
            appItem.DefaultItem = true;

            fRecentFiles = new MenuItem("Recent files");
            fAutorunItem = new MenuItem("Autorun", miAutorun_Click);

            MenuItem[] menu = new MenuItem[] {
                appItem,
                fRecentFiles,
                new MenuItem("-"),
                fAutorunItem,
                new MenuItem("-"),
                new MenuItem("About", miAbout_Click),
                new MenuItem("Exit", miExit_Click)
            };
            return menu;
        }

        private void LoadSettings()
        {
            fMRUFiles.Clear();

            try {
                string iniPath = AppHost.GetAppDataPathStatic() + "GEDKeeper2.ini";
                IniFile ini = new IniFile(iniPath);
                try {
                    int interfaceLang = (ushort)ini.ReadInteger("Common", "InterfaceLang", 0);
                    GlobalOptions.LoadMRUFromFile(ini, fMRUFiles);

                    GlobalOptions.Instance.FindLanguages();
                    GlobalOptions.Instance.LoadLanguage(interfaceLang);
                } finally {
                    ini.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKTray.LoadSettings()", ex);
            }

            fRecentFiles.MenuItems.Clear();
            int num = fMRUFiles.Count;
            for (int i = 0; i < num; i++) {
                var mf = fMRUFiles[i];
                string fn = mf.FileName;

                MenuItem mi = new MenuItem(fn);
                mi.Tag = mf;
                mi.Click += MRUFileClick;
                fRecentFiles.MenuItems.Add(mi);
            }
            fRecentFiles.Enabled = (num > 0);
        }

        private void MRUFileClick(object sender, EventArgs e)
        {
            MRUFile mf = (MRUFile)((MenuItem)sender).Tag;
            string appPath = GKUtils.GetAppPath() + "GEDKeeper2.exe";
            GKUtils.LoadExtFile(appPath, mf.FileName);
        }

        private async void LoadEvents()
        {
            int num = fMRUFiles.Count;
            for (int i = 0; i < num; i++) {
                var mf = fMRUFiles[i];
                string gedFileName = mf.FileName;

                try {
                    using (var baseContext = new BaseContext(null)) {
                        bool isLoaded = await baseContext.FileLoad(gedFileName, false, false, false);
                        if (isLoaded) {
                            baseContext.CollectTips(fTipsList);
                        }
                    }
                } catch (Exception ex) {
                    Logger.WriteError("GKTray.LoadEvents(" + gedFileName + ")", ex);
                }
            }

            for (int i = fTipsList.Count - 1; i >= 0; i--) {
                string tip = fTipsList[i];
                if (string.IsNullOrEmpty(tip) || tip[0] == '#') {
                    fTipsList.Delete(i);
                } else {
                    var tipInfo = new TipInfo();
                    tipInfo.LastTime = DateTime.FromBinary(0);

                    fTipsList.SetObject(i, tipInfo);
                }
            }
        }

        private void TimerCallback(object state)
        {
            DateTime timeNow = DateTime.Now;

            for (int i = 0; i < fTipsList.Count; i++) {
                var tipInfo = fTipsList.GetObject(i) as TipInfo;
                TimeSpan elapsedSpan = timeNow.Subtract(tipInfo.LastTime);
                if (elapsedSpan.TotalDays >= 1) {
                    tipInfo.LastTime = timeNow;

                    fNotifyIcon.BalloonTipText = fTipsList[i];
                    fNotifyIcon.ShowBalloonTip(5000);

                    break;
                }
            }
        }

        private void miAutorun_Click(object sender, EventArgs e)
        {
            if (fAutorunItem.Checked) {
                UIHelper.UnregisterStartup();
            } else {
                UIHelper.RegisterStartup();
            }

            fAutorunItem.Checked = UIHelper.IsStartupItem();
        }

        private void miAbout_Click(object sender, EventArgs e)
        {
            using (var dlg = new AboutDlg()) {
                dlg.ShowDialog();
            }
        }

        private void miExit_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        private void Icon_DoubleClick(object sender, EventArgs e)
        {
            string appPath = GKUtils.GetAppPath() + "GEDKeeper2.exe";
            GKUtils.LoadExtFile(appPath);
        }

        #endif

        [STAThread]
        public static void Main(string[] args)
        {
            #if !MONO
            AppHost.CheckPortable(args);

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            bool isFirstInstance;
            using (Mutex mtx = new Mutex(true, "GKTray", out isFirstInstance)) {
                if (isFirstInstance) {
                    GKTray notificationIcon = new GKTray();
                    notificationIcon.fNotifyIcon.Visible = true;
                    Application.Run();
                    notificationIcon.fNotifyIcon.Dispose();
                }
            }
            #endif
        }
    }
}
