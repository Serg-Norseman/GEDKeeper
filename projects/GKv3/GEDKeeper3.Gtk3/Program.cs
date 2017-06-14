using System;
using System.Reflection;
using Eto;
using Eto.Forms;
using GKCommon;
using GKCore;
using GKUI;
using GKUI.Components;

[assembly: AssemblyTitle("GEDKeeper3.Gtk3")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009-2017 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyVersion("3.0.0.0")]

namespace GEDKeeper3.Gtk3
{
    public class Program
    {
        [STAThread]
        public static void Main(string[] args)
        {
            Logger.LogInit(GKUtils.GetLogFilename());
            WinFormsAppHost.ConfigureBootstrap(false);

            AppHost.InitSettings();
            try
            {
                var appHost = (WinFormsAppHost)AppHost.Instance;
                appHost.Init(args, false);

                new Application(Platforms.Gtk3).Run(new BaseWinSDI());
            } finally {
                AppHost.DoneSettings();
            }
        }
    }
}
