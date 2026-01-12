/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using System.Security.Permissions;
using System.Windows.Forms;
using GKCore;
using GKCore.Utilities;
using GKUI.Platform;

[assembly: AssemblyTitle("GEDKeeper")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct(GKData.APP_TITLE)]
[assembly: AssemblyCopyright(GKData.APP_COPYRIGHT)]
[assembly: AssemblyVersion(GKData.APP_VERSION_2X)]
[assembly: AssemblyCulture("")]

namespace GKUI
{
    /// <summary>
    /// The main startup class of application.
    /// </summary>
    public static class GKProgram
    {
        [STAThread]
        [SecurityPermission(SecurityAction.Demand, Flags=SecurityPermissionFlag.ControlAppDomain)]
        public static void Main(string[] args)
        {
#if OS_MSWIN
            if (ShellLinkTool.HasArg(args)) {
                ShellLinkTool.CreateShortcut();
                return;
            }
#endif

            WFAppHost.Startup(args);

            using (var tracker = new SingleInstanceTracker(GKData.APP_TITLE, AppHost.GetSingleInstanceEnforcer)) {
                if (tracker.IsFirstInstance) {
                    AppHost.InitSettings();
                    var appHost = (WFAppHost)AppHost.Instance;
                    appHost.Init(args, false);
                    Application.Run();
                } else {
                    tracker.SendMessageToFirstInstance(args);
                }
            }
        }
    }
}
