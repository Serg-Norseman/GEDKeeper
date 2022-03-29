using System;
using System.Reflection;
using Eto;
using Eto.Forms;
using GKCore;
using GKUI.Platform;

[assembly: AssemblyTitle("GEDKeeper3.Mac")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009-2017 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyVersion("3.0.0.0")]

namespace GEDKeeper3.Mac
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
            //Style.Add<ButtonToolItemHandler>("icons", h => h.Widget.Image.);
            //Style.Add<ButtonHandler>("icons", h => h.Widget.Image.);

            EtoAppHost.ConfigureBootstrap(false);
            AppHost.CheckPortable(args);
            Logger.Init(AppHost.GetLogFilename());
            LogSysInfo();

            var application = new Application(Platforms.Mac64);

            AppHost.InitSettings();
            try {
                var appHost = (EtoAppHost) AppHost.Instance;
                appHost.Init(args, false);

                application.Run();
            } finally {
                AppHost.DoneSettings();
            }
        }
    }
}
