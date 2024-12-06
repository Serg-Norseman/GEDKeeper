/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using Eto.Forms;
using GKCore;
using GKCore.SingleInstance;
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

            application.Platform.Add<GKButtonToolItem.IHandler>(() => new GKButtonToolItemHandler());
            application.Platform.Add<GKDropDownToolItem.IHandler>(() => new GKDropDownToolItemHandler());
            application.Platform.Add<NativeHostControl.IHandler>(() => new NativeHostControlHandler());

            using (var tracker = new SingleInstanceTracker(GKData.APP_TITLE, AppHost.GetSingleInstanceEnforcer)) {
                if (tracker.IsFirstInstance) {
                    AppHost.InitSettings();
                    try {
                        var appHost = (EtoAppHost)AppHost.Instance;
                        appHost.Init(args, false);

                        application.Run();
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
