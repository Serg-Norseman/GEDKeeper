using System;
using System.Reflection;
using Eto;
using Eto.Forms;
using GKCore;
using GKUI;
using Eto.Wpf.Forms.ToolBar;
using Eto.Wpf.Forms.Controls;

[assembly: AssemblyTitle("GEDKeeper3.Wpf")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009-2019 by Sergey V. Zhdanovskih")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyVersion("3.0.0.0")]

namespace GEDKeeper3.Wpf
{
    public class Program
    {
        private static void LogSysInfo()
        {
            try
            {
                //#if __MonoCS__
                //Logger.LogWrite("Mono Version: " + SysUtils.GetMonoVersion());
                //Logger.LogWrite("Desktop Type: " + SysUtils.GetDesktopType().ToString());
                //#endif

                // There should be no links to the application infrastructure
                Assembly execAssembly = Assembly.GetExecutingAssembly();
                Logger.LogWrite("CLR Version: " + execAssembly.ImageRuntimeVersion);
                Logger.LogWrite("GK Version: " + execAssembly.GetName().Version.ToString());
            }
            catch { }
        }

        [STAThread]
        public static void Main(string[] args)
        {
            //Style.Add<ButtonToolItemHandler>("icons", h => h.Widget.Image.);
            //Style.Add<ButtonHandler>("icons", h => h.Widget.Image.);

            EtoAppHost.ConfigureBootstrap(false);
            AppHost.CheckPortable(args);
            Logger.LogInit(EtoAppHost.GetLogFilename());
            LogSysInfo();

            var application = new Application(Platforms.Wpf);

            AppHost.InitSettings();
            try
            {
                var appHost = (EtoAppHost)AppHost.Instance;
                appHost.Init(args, false);

                application.Run();
            } finally {
                AppHost.DoneSettings();
            }
        }
    }
}
