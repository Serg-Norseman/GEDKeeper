/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using Eto;
using Eto.Forms;
using GKCore;
using GKUI.Platform;

[assembly: AssemblyTitle("GEDKeeper3.Wpf")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright(GKData.APP_COPYRIGHT)]
[assembly: AssemblyVersion("3.0.0.0")]
[assembly: AssemblyCulture("")]

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
                //#if MONO
                //Logger.LogWrite("Mono Version: " + SysUtils.GetMonoVersion());
                //Logger.LogWrite("Desktop Type: " + SysUtils.GetDesktopType().ToString());
                //#endif

                // There should be no links to the application infrastructure
                Assembly execAssembly = Assembly.GetExecutingAssembly();
                Logger.WriteInfo("CLR Version: " + execAssembly.ImageRuntimeVersion);
                Logger.WriteInfo("GK Version: " + execAssembly.GetName().Version.ToString());
            } catch {
                // dummy
            }
        }

        [STAThread]
        public static void Main(string[] args)
        {
#if NETCOREAPP3_1
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
#endif
            //Style.Add<ButtonToolItemHandler>("icons", h => h.Widget.Image.);
            //Style.Add<ButtonHandler>("icons", h => h.Widget.Image.);

            EtoAppHost.ConfigureBootstrap(false);
            AppHost.CheckPortable(args);
            Logger.Init(AppHost.GetLogFilename());
            LogSysInfo();

            var application = new Application(Platforms.Wpf);

            AppHost.InitSettings();
            try {
                var appHost = (EtoAppHost)AppHost.Instance;
                appHost.Init(args, false);

                application.Run();
            } finally {
                AppHost.DoneSettings();
            }
        }
    }
}
