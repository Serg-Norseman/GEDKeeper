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
using System.Reflection;
using System.Resources;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Threading;
using System.Windows.Forms;

using GKCore;
using GKCore.SingleInstance;

[assembly: AssemblyTitle("GEDKeeper")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009-2018 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("2.13.2.0")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]
[assembly: CLSCompliant(true)]
[assembly: ComVisible(false)]
[assembly: NeutralResourcesLanguage("en")]

namespace GKUI
{
    /// <summary>
    /// The main startup class of application.
    /// </summary>
    public static class GKProgram
    {
        private static void LogSysInfo()
        {
            try {
                #if __MonoCS__
                Logger.LogWrite("Mono Version: " + SysUtils.GetMonoVersion());
                Logger.LogWrite("Desktop Type: " + SysUtils.GetDesktopType().ToString());
                #endif

                // There should be no links to the application infrastructure
                Assembly execAssembly = Assembly.GetExecutingAssembly();
                Logger.LogWrite("CLR Version: " + execAssembly.ImageRuntimeVersion);
                Logger.LogWrite("GK Version: " + execAssembly.GetName().Version.ToString());
            } catch {
                // dummy
            }
        }

        [STAThread]
        [SecurityPermission(SecurityAction.Demand, Flags=SecurityPermissionFlag.ControlAppDomain)]
        private static void Main(string[] args)
        {
            WinFormsAppHost.ConfigureBootstrap(false);
            AppHost.CheckPortable(args);
            Logger.LogInit(WinFormsAppHost.GetLogFilename());
            LogSysInfo();

            Application.ThreadException += ExExceptionHandler;
            Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException, true);
            AppDomain.CurrentDomain.UnhandledException += UnhandledExceptionsHandler;

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            using (SingleInstanceTracker tracker = new SingleInstanceTracker(GKData.APP_TITLE, GetSingleInstanceEnforcer))
            {
                if (tracker.IsFirstInstance) {
                    AppHost.InitSettings();
                    try
                    {
                        var appHost = (WinFormsAppHost)AppHost.Instance;
                        appHost.Init(args, false);

                        Application.Run(appHost.AppContext);
                    } finally {
                        AppHost.DoneSettings();
                    }
                } else {
                    tracker.SendMessageToFirstInstance(args);
                }
            }
        }

        private static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
        {
            return AppHost.Instance;
        }

        private static void ExExceptionHandler(object sender, ThreadExceptionEventArgs args)
        {
            Logger.LogWrite("GK.ExExceptionHandler(): " + args.Exception.Message);
            Logger.LogWrite("GK.ExExceptionHandler(): " + args.Exception.StackTrace);
        }

        private static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs args)
        {
            // Saving the copy for restoration
            AppHost.Instance.CriticalSave();

            Exception e = (Exception) args.ExceptionObject;
            Logger.LogWrite("GK.UnhandledExceptionsHandler(): " + e.Message);
            Logger.LogWrite("GK.ExExceptionHandler(): " + e.StackTrace);
        }
    }
}
