using System;
using System.Reflection;
using Eto;
using Eto.Forms;
using GKCommon;
using GKCore;
using GKUI.Components;

[assembly: AssemblyTitle("GEDKeeper3.WinForms")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009-2017 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyVersion("3.0.0.0")]

namespace GEDKeeper3.WinForms
{
    public class Program
    {
        [STAThread]
        public static void Main(string[] args)
        {
            Logger.LogInit(GKUtils.GetLogFilename());
            EtoFormsAppHost.ConfigureBootstrap(false);

            AppHost.InitSettings();
            try
            {
                var application = new Application(Platforms.WinForms);

                var appHost = (EtoFormsAppHost)AppHost.Instance;
                appHost.Init(args, false);

                application.Run();
            } finally {
                AppHost.DoneSettings();
            }
        }
    }
}
