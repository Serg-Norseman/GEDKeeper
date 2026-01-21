/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Reflection;
using GKCore;
using GKCore.Utilities;
using GKUI.Forms;
using GKUI.Platform;
using Terminal.Gui;

[assembly: AssemblyTitle("GEDKeeperT")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct(GKData.APP_TITLE)]
[assembly: AssemblyCopyright(GKData.APP_COPYRIGHT)]
[assembly: AssemblyVersion(GKData.APP_VERSION_3X)]
[assembly: AssemblyCulture("")]

namespace GEDKeeperT
{
    /// <summary>
    /// The main startup class of application.
    /// </summary>
    public static class GKProgram
    {
        public static void Main(string[] args)
        {
            Application.Init();

            TGAppHost.Startup(args);

            using (var tracker = new SingleInstanceTracker(GKData.APP_TITLE, AppHost.GetSingleInstanceEnforcer)) {
                if (tracker.IsFirstInstance) {
                    AppHost.InitSettings();
                    try {
                        var appHost = (TGAppHost)AppHost.Instance;
                        appHost.Init(args, false);

                        var win = new BaseWinSDI();
                        Application.Top.Add(win);

                        Application.Run();
                        Application.Shutdown();
                    } finally {
                        AppHost.DoneSettings();
                    }
                } else {
                    tracker.SendMessageToFirstInstance(args);
                }
            }
        }
    }
}
