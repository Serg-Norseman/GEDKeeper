using System;
using System.Reflection;
using System.Resources;
using Eto;
using Eto.Forms;
using GKCommon;
using GKCore;
using GKUI;
using GKUI.Components;

[assembly: AssemblyTitle("GEDKeeper3.Gtk2")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009-2017 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyVersion("3.0.0.0")]
[assembly: NeutralResourcesLanguage("en")]

namespace GEDKeeper3.Gtk2
{
    public class Program
    {
        [STAThread]
        public static void Main(string[] args)
        {
            Logger.LogInit(EtoFormsAppHost.GetLogFilename());
            EtoFormsAppHost.ConfigureBootstrap(false);

            AppHost.InitSettings();
            try
            {
                var application = new Application(Platforms.Gtk2);

                var appHost = (EtoFormsAppHost)AppHost.Instance;
                appHost.Init(args, false);

                application.Run();
            } finally {
                AppHost.DoneSettings();
            }
        }
    }
}
