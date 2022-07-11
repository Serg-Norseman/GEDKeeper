/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.Reflection;
using GKCore;
using GKCore.SingleInstance;
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
            TGAppHost.Startup(args);

            Application.Init();

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
