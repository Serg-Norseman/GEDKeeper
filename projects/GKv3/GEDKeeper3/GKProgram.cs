/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using Eto.Forms;
using GKCore;
using GKCore.Utilities;
using GKUI.Platform;

[assembly: AssemblyTitle("GEDKeeper3")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct(GKData.APP_TITLE)]
[assembly: AssemblyCopyright(GKData.APP_COPYRIGHT)]
[assembly: AssemblyVersion(GKData.APP_VERSION_3X)]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GEDKeeper3
{
    /// <summary>
    /// The main startup class of application.
    /// </summary>
    public static class GKProgram
    {
        [STAThread]
        public static void Main(string[] args)
        {
#if OS_MSWIN
            if (ShellLinkTool.HasArg(args)) {
                ShellLinkTool.CreateShortcut();
                return;
            }
#endif

            EtoAppHost.Startup(args);

            var application = new Application();
            application.UnhandledException += (_, eventArgs) => {
                var ex = eventArgs.ExceptionObject as Exception ??
                          new Exception(eventArgs.ExceptionObject?.ToString());
                Logger.WriteError(string.Format("Unhandled exception (IsTerminating={0}):", eventArgs.IsTerminating), ex);
            };
            EtoAppStyles.InitPlatformHandlers(application);

            using (var tracker = new SingleInstanceTracker(GKData.APP_TITLE, AppHost.GetSingleInstanceEnforcer)) {
                if (tracker.IsFirstInstance) {
                    AppHost.InitSettings();
                    var appHost = (EtoAppHost)AppHost.Instance;
                    appHost.Init(args, false);
                    application.Run();
                } else {
                    tracker.SendMessageToFirstInstance(args);
                }
            }
        }
    }
}
