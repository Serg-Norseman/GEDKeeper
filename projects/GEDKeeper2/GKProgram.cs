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
using System.Reflection;
using System.Resources;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Threading;
using System.Windows.Forms;

using Externals.SingleInstancing;
using GKCommon;
using GKCore;

[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2009-2016 Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyTitle("GEDKeeper2")]
[assembly: AssemblyVersion("2.7.0.0")]
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: NeutralResourcesLanguage("en")]

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    internal sealed class GKProgram
    {
        #if __MonoCS__
        private static MainWin fMainWin;
        #endif

        [STAThread]
        [SecurityPermission(SecurityAction.Demand, Flags=SecurityPermissionFlag.ControlAppDomain)]
        private static void Main(string[] args)
        {
            Application.ThreadException += ExExceptionHandler;
            Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException, true);
            AppDomain.CurrentDomain.UnhandledException += UnhandledExceptionsHandler;
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            #if __MonoCS__
            
            try {
                bool firstInstance = GlobalMutexPool.CreateMutex(GKData.APP_TITLE, true);
                if (!firstInstance) {
                    ActivatePreviousInstance(args);
                } else {
                    IpcBroadcast.StartServer();
                    fMainWin = new MainWin();
                    fMainWin.SetArgs(args);
                    Application.Run(fMainWin);
                }

                IpcBroadcast.StopServer();
                GlobalMutexPool.ReleaseAll();
            } finally {
            }

            #else
            SingleInstanceTracker tracker = null;
            try
            {
                tracker = new SingleInstanceTracker(GKData.APP_TITLE, GetSingleInstanceEnforcer);

                if (tracker.IsFirstInstance) {
                    MainWin fmMain = (MainWin)tracker.Enforcer;
                    fmMain.SetArgs(args);
                    Application.Run(fmMain);
                } else {
                    tracker.SendMessageToFirstInstance(args);
                }
            }
            finally
            {
                if (tracker != null) tracker.Dispose();
            }
            #endif
        }

        #if __MonoCS__
        public static void ProcessMessage(IpcMessage msg)
        {
            if (msg.Message == AppMessage.RestoreWindow) {
                fMainWin.WindowState = FormWindowState.Normal;
            } else if (msg.Message == AppMessage.IpcByFile) {
                fMainWin.WindowState = FormWindowState.Normal;
                IpcBroadcast.ProcessGlobalMessage(msg.LParam, fMainWin);
            }
        }

        private static void ActivatePreviousInstance(string[] args)
        {
            try
            {
                if (args.Length == 0 || string.IsNullOrEmpty(args[0]))
                {
                    IpcBroadcast.Send(AppMessage.RestoreWindow, 0, false);
                }
                else
                {
                    IpcParamEx ipcMsg = new IpcParamEx(IpcBroadcast.CmdOpenDatabase, IpcBroadcast.SafeSerialize(args));
                    IpcBroadcast.SendGlobalMessage(ipcMsg);
                }
            }
            catch (Exception) { }
        }
        #endif

        private static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
        {
            return new MainWin();
        }

        static void ExExceptionHandler(object sender, ThreadExceptionEventArgs args)
        {
            Logger.LogWrite("GK.ExExceptionHandler(): " + args.Exception.Message);
            Logger.LogWrite("GK.ExExceptionHandler(): " + args.Exception.StackTrace);
        }

        static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs args)
        {
            // saving restore copies
            MainWin.Instance.CriticalSave();

            Exception e = (Exception) args.ExceptionObject;
            Logger.LogWrite("GK.UnhandledExceptionsHandler(): " + e.Message);
            Logger.LogWrite("GK.ExExceptionHandler(): " + e.StackTrace);
        }
    }
}
