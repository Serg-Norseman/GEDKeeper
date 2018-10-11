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
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using BSLib;
using GKCore;
using GKCore.Options;

[assembly: AssemblyTitle("GKTray")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2018 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("0.1.0.0")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]

namespace GKTray
{
    public sealed class GKTray
    {
        public const string APP_TITLE = "GEDKeeper Tray";

        private ContextMenu fMenu;
        private NotifyIcon fNotifyIcon;
        private MenuItem fRecentFiles;

        public GKTray()
        {
            fMenu = new ContextMenu(InitializeMenu());

            fNotifyIcon = new NotifyIcon();
            fNotifyIcon.DoubleClick += Icon_DoubleClick;
            fNotifyIcon.Icon = new Icon(GKUtils.LoadResourceStream("Resources.icon_gedkeeper.ico"));
            fNotifyIcon.ContextMenu = fMenu;

            LoadSettings();
        }

        private MenuItem[] InitializeMenu()
        {
            var appItem = new MenuItem("GEDKeeper", Icon_DoubleClick);
            appItem.DefaultItem = true;

            fRecentFiles = new MenuItem("Recent files");

            MenuItem[] menu = new MenuItem[] {
                appItem,
                fRecentFiles,
                new MenuItem("-"),
                new MenuItem("About", miAbout_Click),
                new MenuItem("Exit", miExit_Click)
            };
            return menu;
        }

        private void LoadSettings()
        {
            List<MRUFile> mruFiles = new List<MRUFile>();

            try {
                string iniPath = AppHost.GetAppDataPathStatic() + "GEDKeeper2.ini";
                IniFile ini = new IniFile(iniPath);
                try {
                    GlobalOptions.LoadMRUFromFile(ini, mruFiles);
                } finally {
                    ini.Dispose();
                }
            } catch (Exception ex) {
                Logger.LogWrite("GKTray.LoadSettings(): " + ex.Message);
            }

            fRecentFiles.MenuItems.Clear();
            int num = mruFiles.Count;
            for (int i = 0; i < num; i++) {
                var mf = mruFiles[i];
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

        [STAThread]
        public static void Main(string[] args)
        {
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
        }
    }
}
